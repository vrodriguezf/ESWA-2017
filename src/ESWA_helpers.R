#
# General clustering validation given a dissimilarity matrix (x)
#
# Valid clMethods: PAM,hclust
#
ESWA_helper.clusteringValidation <- function (dissMatrix,
                                              clustMethods=list(c(name="PAM",command=pam,params=NULL)),
                                              K=2:8,
                                              returnAllResults=FALSE) {
  clResults = lapply(clustMethods, function (clustMethod) {
    print(paste("\tApplying clustering method: ",clustMethod["name"]))
    list(
      clustMethod=clustMethod$name,
      results = lapply(K, function (k) {
        if (clustMethod$name == "PAM") {
          aux <- pam(x=dissMatrix,k=k,diss=TRUE)
          clustering <- aux$clustering
          medoids <- aux$medoids
        }
        else if (clustMethod$name == "hclust")
          clustering <- cutree(hclust(d = dissMatrix, method="complete"), k=k)
        else
          stop(paste("Clustering Method [",clustMethod$name,"] not recognized"))
        
        list(
          k=k,
          clust=clustering,
          medoids=medoids,
          stats=cluster.stats(d = dissMatrix,clustering = clustering, G2 = TRUE, G3 = TRUE, wgap = TRUE,sepindex = TRUE,silhouette = TRUE)
        )
      })
    )
  })
  
  summary <- do.call(rbind,lapply(clResults, function (resultByClustMethod) {
    
    kValDf <- do.call(rbind,lapply(resultByClustMethod$results, function (resultByK) {
      c(
        #dunn=resultByK$stats$dunn2,
        ASW=resultByK$stats$avg.silwidth,
        CH=resultByK$stats$ch,
        PH=resultByK$stats$pearsongamma
        #sindex=resultByK$stats$sindex,
        #widestgap=resultByK$stats$stats$widestgap
      )    
    }))
    rownames(kValDf) <- K
    kValDf <- melt(as.matrix(kValDf),varnames = c("k","valMetric"))
    kValDf <- cbind(clustMethod = resultByClustMethod$clustMethod, kValDf)
    
    kValDf
  }))
  
  #Cast the data frame
  summary <- dcast(summary, clustMethod + k ~ valMetric)
  
  #Transform infinites to NAs
  is.na(summary) <- do.call(cbind,lapply(summary, is.infinite))
  
  summary <- summary %>% 
    mutate(ASWbar=ASW/max(ASW),
           CHbar=CH/max(CH),
           PHbar=PH/max(PH)) %>% 
    mutate(valRating=rowSums(.[c("ASWbar","CHbar","PHbar")],na.rm = TRUE))
  
  # aux <- summary %>% 
  #   transform(valRating = ifelse(!is.na(max(ASW)),ASW/max(ASW),0) + 
  #               ifelse(!is.na(max(CH)),CH/max(CH),0) + 
  #               ifelse(!is.na(max(PH)),PH/max(PH),0)) %>%
  #   filter(valRating == max(valRating)) %>%
  #   head(n=1)
  
  if (returnAllResults) {
    result <- lapply(1:nrow(summary),function (rowIndex) {
      aux <- summary[rowIndex,]
      clustMethodIndex <- which((laply(clustMethods, function (cm) cm["name"])) == aux$clustMethod)
      kIndex <- which((laply(K, function (k) k == aux$k)))
      list(
        stats = as.list(aux),
        clustering = clResults[[clustMethodIndex]]$results[[kIndex]]$clust,
        medoids = clResults[[clustMethodIndex]]$results[[kIndex]]$medoids
      )
    }) %>% setNames(summary$k)
  } else {
    aux <- summary %>% 
      filter(valRating == max(valRating)) %>%
      head(n=1)
    
    clustMethodIndex <- which((laply(clustMethods, function (cm) cm["name"])) == aux$clustMethod)
    kIndex <- which((laply(K, function (k) k == aux$k)))
    result <- list(
      stats = as.list(aux),
      clustering = clResults[[clustMethodIndex]]$results[[kIndex]]$clust,
      medoids = clResults[[clustMethodIndex]]$results[[kIndex]]$medoids
    )
  }
  
  # return(list(
  #   validationSummary=summary,
  #   bestClust=simClust
  # ))
  return(result)
}



##
# Use irr package
# Note: Needed large amount of data to make it work
##
ESWA_helper.kendallCoefficientOfConcordance <-
  function(humanGroundTruth) {
    #Montar una matriz nxm (subjects vs. raters)
    # A subject is the combination sim1_ sim2_ performanceMeasure

    ratings <- humanGroundTruth %>%
      mutate(sim1_ = pmin(sim1, sim2), sim2_ = pmax(sim1, sim2)) %>%
      dplyr::select(-sim1,-sim2) %>% 
      group_by(sim1_, sim2_,performanceMeasure) %>% 
      nest(-`_id`) %>% 
      mutate(data=data %>% purrr::map(~ dplyr::distinct(.,labeler,.keep_all = TRUE))) %>% 
      unnest() %>%
      mutate(similarityRating=as.integer(similarityRating)) %>% 
      spread(key = "labeler",value = "similarityRating") %>% 
      select(-sim1_,-sim2_,-performanceMeasure) 
    
    if (nrow(na.omit(ratings)) == 0) {
      stop("No shared ratings found among all subjects!")
    }
    
    print(paste("Calculating Kendall's W over ",nrow(na.omit(ratings)),"shared rated subjects"))
    print(na.omit(ratings))
    irr::kendall(ratings = ratings, correct = FALSE)
  }

##
# TODO: Add connection params?
##
ESWA_helper.loadHumanGroundTruth <- function(cache = T) {
  
  if (cache & base::exists("humanGroundTruth"))
  {
    return(humanGroundTruth)
  }
  else 
  {
    stop("Permission denied. Contact the authors to access the database")
    # host <- ""
    # db <- ""
    # humanGroundTruthNs <- paste(db,"humanGroundTruth",sep=".")
    # dwrConn <- mongo.create(host = host, db="admin", username = "admin", password = "mongoPassword1620")
    # humanGroundTruth <- mongo.find.all(mongo = dwrConn,ns = humanGroundTruthNs,data.frame = TRUE) %>% as_data_frame()
    # mongo.destroy(dwrConn)
    # ProjectTemplate::cache(variable = "humanGroundTruth")
    # return(humanGroundTruth)
  }
}

#
# Input: human ground truth, as it comes from the database
# Output: 
#
ESWA_helper.filterHumanGroundTruth <- function(df) {
  df %>%
    filter(
      labeler!="Test",
      sim1 %in% names(narrowSimSeries),
      sim2 %in% names(narrowSimSeries)
    ) %>%
    rowwise() %>% 
    filter(
      difftime(mongo.oid.time(mongo.oid.from.string(`_id`)),make_datetime(year=2016,month=9L,day=16L)) > 0
    ) %>% 
    ungroup() %>% 
    as_data_frame()
}

#
# Input: human groundtruth, as it comes from the database
# Output: Results prepared for clustering evaluation
#
ESWA_helper.preprocessHumanGroundTruth <- function(df) {
  df %>%
    mutate(sim1_ = pmin(sim1,sim2), sim2_ = pmax(sim1,sim2)) %>%
    group_by(sim1_,sim2_) %>%
    filter(length(unique(performanceMeasure)) > Constants$HUMAN_GROUND_TRUTH$MIN_PERFORMANCE_MEASURES_RATED_PER_SIMULATION) %>% 
    dplyr::summarise(
      avgRating = mean(similarityRating)
    )  
}


##
# External validation of clustering results using human ground truth (ESWA experiment)
# Compare clustering assignation by pairs of human assignments
# Input:  clusteringResults: Named vector of clustering results (names are simulation IDs)
#         humanGroundTruth: Human Groud truth Dataframe, as obtained from the MongoDB database
# Output: Pairwise Accuracy (0-100%) of agreement between prediction and ground truth
##
ESWA_helper.clusteringEvaluation <- function(clusteringResults,humanGroundTruth) {
  humanGroundTruth_ <- ESWA_helper.preprocessHumanGroundTruth(humanGroundTruth)
  
  results <- humanGroundTruth_ %>% alply(1, function (evaluation) {
    groundTruthMatch <- ifelse(
      evaluation$avgRating >= Constants$HUMAN_GROUND_TRUTH$ACCEPT_MATCH_RATING_THRESHOLD,
      TRUE,
      ifelse(
        evaluation$avgRating <= Constants$HUMAN_GROUND_TRUTH$REJECT_MATCH_RATING_THRESHOLD,
        FALSE,
        NA
      )
    )
    predictedMatch <- clusteringResults[evaluation$sim1_] == clusteringResults[evaluation$sim2_]
    return(ifelse(groundTruthMatch == predictedMatch,1,0))
  })

  return((results %>% unlist %>% mean(na.rm = TRUE))*100)
}

##
#
##
ESWA_helper.calculate_evaluatedShowdowns <- function() {
  #Load and filter current human ground truth
  humanGroundTruth <- ESWA_helper.loadHumanGroundTruth() %>% ESWA_helper.filterHumanGroundTruth()
  
  #Calculate the different combinations of sim1-sim2-performanceMeasure found
  humanGroundTruth %>% 
    mutate(sim1_ = pmin(sim1,sim2), sim2_ = pmax(sim1,sim2)) %>% 
    group_by(sim1_,sim2_,performanceMeasure) %>% 
    summarise(n()) %>% 
    nrow 
}