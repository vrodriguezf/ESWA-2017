#
# This scripts take the object "simSeries", i.e, the processed performance measures from the simulations in DWR,
# and perform time series clustering for every performance measure.
#
#
library('ProjectTemplate')
load.project()
SAVE_PLOT <- FALSE
SAVE_OUTPUT<- TRUE

#Check dependencies with previos source script files (metricsTimeSeries)
if (!exists("simSeries")) {
  source("./src/ESWA/ESWA_1-TimeSeriesMeasures.R")
}

#TS clustering (dissimilarity measures, clustering methods, internal validation measures)
TS <- list(
  score=lapply(simSeries,function (simTS) simTS$scoreTS),
  agility=lapply(simSeries,function (simTS) simTS$agilityTS),
  attention=lapply(simSeries,function (simTS) simTS$attentionTS),
  cooperation=lapply(simSeries,function (simTS) simTS$cooperationTS),
  aggressiveness=lapply(simSeries,function (simTS) simTS$aggressivenessTS),
  precision=lapply(simSeries,function (simTS) simTS$precisionTS)
)


#Dissimilarity methods to use
dissMethods <- list(c(name="FRECHET",command="FRECHET",params=NULL),
                    c(name="DTWARP",command="DTWARP",params=NULL))
                    #c(name="NCD",command="NCD",params=NULL))

#Clustering methods to use
clustMethods <- list(c(name="PAM",command=pam,params=NULL),
                     c(name="hclust",command=hclust,params=NULL))
                     #c(name="diana",command=diana,params=NULL))

#Number of clusters for each time series clustering
K <- 2:8


validationResults <- list()
for (i in 1:length(TS)) {
  
  dataTS <- TS[[i]]
  
  clResults <- lapply(dissMethods, function (dissMethod) {
    print(paste("Applying dissimilarity measure: ",dissMethod["name"]))
    dissMatrix <- diss(SERIES = dataTS, METHOD = dissMethod["command"])
    dissMatrix <- as.dist(dissMatrix)
    
    list(
      method = dissMethod["name"],
      data = dissMatrix,
      results = lapply(clustMethods, function (clustMethod) {
        print(paste("\tApplying clustering meethod: ",clustMethod["name"]))
        list(
          clustMethod=clustMethod$name,
          results = lapply(K, function (k) {
            if (clustMethod$name == "PAM")
              clustering <- pam(x=dissMatrix,k=k,diss=TRUE)$clustering
            else if (clustMethod$name == "hclust")
              clustering <- cutree(hclust(d = dissMatrix, method="complete"), k=k)
            else if (clustMethod$name == "diana")
              clustering <- diana(x = dissMatrix) %>% as.hclust() %>% cutree(k=k)
            else
              stop(paste("Clustering Method [",clustMethod$name,"] not recognized"))
            
            list(
              k=k,
              clust=clustering,
              stats=cluster.stats(d = dissMatrix,clustering = clustering, G2 = TRUE, G3 = TRUE, wgap = TRUE,sepindex = TRUE,silhouette = TRUE)
            )
          })
        )
      })
    )
  })
  
  summary <- do.call(rbind, lapply(clResults, function (resultByDiss) {
    
    
    clustMethodDf <- do.call(rbind,lapply(resultByDiss$results, function (resultByClustMethod) {
      
      kValDf <- do.call(rbind,lapply(resultByClustMethod$results, function (resultByK) {
        c(
          #dunn=resultByK$stats$dunn2,
          clusteringResult=resultByK$clust,
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
    clustMethodDf <- cbind(dissMeasure=resultByDiss$method,clustMethodDf)
    
    clustMethodDf
  }))
  
  #Cast the data frame
  summary <- dcast(summary, dissMeasure + clustMethod + k ~ valMetric)
  
  validationResults[[i]] <- summary  
}
names(validationResults) <- names(TS)


##
#
# Extract the best clusterizations
#
##
TSClustering <- list()
for (i in 1:length(TS)) {
  
  aux <- validationResults[[i]] %>% 
          transform(valRating = ASW/max(ASW) + CH/max(CH) + PH/max(PH)) %>%
          filter(valRating == max(valRating)) %>%
          head(n=1)
  
  dissMethodIndex <- which((laply(dissMethods, function (dm) dm["name"])) == aux$dissMeasure)
  clustMethodIndex <- which((laply(clustMethods, function (cm) cm["name"])) == aux$clustMethod)
  kIndex <- which((laply(K, function (k) k == aux$k)))
  
  TSClustering[[i]] <- list(
    stats = as.list(aux),
    clustering = clResults[[dissMethodIndex]]$results[[clustMethodIndex]]$results[[kIndex]]$clust
  )
}
names(TSClustering) <- names(TS)

#Summary validation results
summaryOfBestResults <- data.frame(lapply(TSClustering, function (tsClust) unlist(tsClust$stats)))
#rownames(summaryOfBestResults) <- names(TS)

#Write validation resuts in csvs
l_ply(names(validationResults), function (tsName) {
  write.csv(validationResults[[tsName]], file = paste("./output/ESWA/",tsName,".csv"),row.names = FALSE)
})
write.csv(summaryOfBestResults,file = "./output/ESWA/summaryOfBestResults.csv",row.names = TRUE)


# Save final clusterization
saveRDS(TSClustering, file = "./output/ESWA/performanceMeasuresClusterization.Rds") #Pass to script 3
saveRDS(TSClustering, file = "./output/ESWA/methodologyPMClusterization.Rds") #Special save
saveRDS(validationResults, file="output/ESWA/validationResults.Rds")
saveRDS(clResults, file="output/ESWA/clResults.Rds")

# Final
print("############################### Script successfully executed :) ##################################")


