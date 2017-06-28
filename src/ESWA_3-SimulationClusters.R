#
# This script starts when "clusteringValidation" finishes
# 
# It takes the best clusterization for each metric and use it yo group the 
# simulations using a assignation dissimilariy matrix
#

if (!exists("PIPELINE_CALL") || PIPELINE_CALL == FALSE) {
  source(file = "./src/ESWA_helpers.R")
  library('ProjectTemplate')
  load.project()
  SAVE_PLOT = FALSE
  SAVE_OUTPUT = TRUE
  EVALUATE_ALL <- TRUE
  clusteringData <- TSClustering
} else {
  # Take the input data as aresult from script "clusteringValidation.R"
  clusteringData <- readRDS(file = "output/ESWA/performanceMeasuresClusterization.Rds")
}

#Clustering methods
clustMethods <- list(c(name="PAM",command=pam,params=NULL))
                     #c(name="hclust",command=hclust,params=NULL))

#Number of clusters
K <- 3:8

##
# START
##
clusterAssignationDf <- data.frame(lapply(clusteringData, function (ts) ts$clustering))

simDiss <- apply(clusterAssignationDf,1,function (sim_i) {
  apply(clusterAssignationDf,1,function (sim_j) {
    return(1-(length(which(sim_i==sim_j))/ncol(clusterAssignationDf)))
  })
})
simDiss <- as.dist(simDiss)

#Extract in the object simClust the best validation results
simClust <- ESWA_helper.clusteringValidation(
  dissMatrix = simDiss,
  clustMethods = list(c(name="PAM",command=pam,params=NULL)),
  K=K,
  returnAllResults = FALSE
)

if (EVALUATE_ALL) {
  step2MethodologyValidationResults <- ESWA_helper.clusteringValidation(
    dissMatrix = simDiss,
    clustMethods = list(c(name="PAM",command=pam,params=NULL)),
    K=K,
    returnAllResults = TRUE
  )
  
  step2MethodologyValidationResults <- lapply(temp, function (simClust) {
    simClust$clustering <- simClust$clustering %>% setNames(names(simSeries))
    simClust
  })
  
  # Evaluate the assignation clusters using the human ground truth
  humanGroundTruth <- ESWA_helper.loadHumanGroundTruth()
  step2MethodologyEvaluationResults <- lapply(step2MethodologyValidationResults, function (x) {
    ESWA_helper.clusteringEvaluation(
      clusteringResults = x$clustering,
      humanGroundTruth = humanGroundTruth)
  })
  
  step2MethodologyEvaluationResults <- data.frame(step2MethodologyEvaluationResults)
  step2MethodologyEvaluationResults
}

# v <- lapply(step2MethodologyValidationResults, function (x) {
#   list(
#     K=x$stats$k,
#     ASW=x$stats$ASW,
#     CH=x$stats$CH,
#     PH=x$stats$PH,
#     VR=x$stats$valRating
#   )
# })
# do.call(rbind.data.frame,v) %>% round(3) %>% write.csv(row.names = FALSE)

# clResults = lapply(clustMethods, function (clustMethod) {
#   print(paste("\tApplying clustering method: ",clustMethod["name"]))
#   list(
#     clustMethod=clustMethod$name,
#     results = lapply(K, function (k) {
#       if (clustMethod$name == "PAM") {
#         aux <- pam(x=simDiss,k=k,diss=TRUE)
#         clustering <- aux$clustering
#         medoids <- aux$medoids
#       }
#       else if (clustMethod$name == "hclust")
#         clustering <- cutree(hclust(d = simDiss, method="complete"), k=k)
#       else
#         stop(paste("Clustering Method [",clustMethod$name,"] not recognized"))
#       
#       list(
#         k=k,
#         clust=clustering,
#         medoids=medoids,
#         stats=cluster.stats(d = simDiss,clustering = clustering, G2 = TRUE, G3 = TRUE, wgap = TRUE,sepindex = TRUE,silhouette = TRUE)
#       )
#     })
#   )
# })
# 
# 
# summary <- do.call(rbind,lapply(clResults, function (resultByClustMethod) {
#   
#   kValDf <- do.call(rbind,lapply(resultByClustMethod$results, function (resultByK) {
#     c(
#       #dunn=resultByK$stats$dunn2,
#       ASW=resultByK$stats$avg.silwidth,
#       CH=resultByK$stats$ch,
#       PH=resultByK$stats$pearsongamma
#       #sindex=resultByK$stats$sindex,
#       #widestgap=resultByK$stats$stats$widestgap
#     )    
#   }))
#   rownames(kValDf) <- K
#   kValDf <- melt(as.matrix(kValDf),varnames = c("k","valMetric"))
#   kValDf <- cbind(clustMethod = resultByClustMethod$clustMethod, kValDf)
#   
#   kValDf
# }))
# 
# #Cast the data frame
# summary <- dcast(summary, clustMethod + k ~ valMetric)
# 
# #Transform infinites to NAs
# is.na(summary) <- do.call(cbind,lapply(summary, is.infinite))
# 
# summary <- summary %>% 
#   mutate(ASWbar=ASW/max(ASW),
#          CHbar=CH/max(CH),
#          PHbar=PH/max(PH)) %>% 
#   mutate(valRating=rowSums(.[c("ASWbar","CHbar","PHbar")],na.rm = TRUE))
# 
# aux <- summary %>% 
#   filter(valRating == max(valRating)) %>%
#   head(n=1)
# 
# print(aux)
# 
# clustMethodIndex <- which((laply(clustMethods, function (cm) cm["name"])) == aux$clustMethod)
# kIndex <- which((laply(K, function (k) k == aux$k)))
# 
# simClust <- list(
#   stats = as.list(aux),
#   clustering = clResults[[clustMethodIndex]]$results[[kIndex]]$clust,
#   medoids = clResults[[clustMethodIndex]]$results[[kIndex]]$medoids
# )

# Evaluate the assignation clusters using the human ground truth
# humanGroundTruth <- ESWA_helper.loadHumanGroundTruth()
# 
# result <- ESWA_helper.clusteringEvaluation(
#   clusteringResults = simClust$clustering %>% setNames(names(simSeries)),
#   humanGroundTruth = humanGroundTruth)

#print(paste("##### Ground truth evaluation [",result," % ] ############"))

#Write data
if (SAVE_OUTPUT) write.csv(summary,file = "./output/ESWA/simulationClusterValidation.csv",row.names = FALSE)

#Plot results
if (SAVE_PLOT) {
  for (i in 1:length(simClust$medoids)) {
    plot <- plotHelper.simulationTS(simSeries[simClust$medoids[i]],
                                    simulationID = simClust$medoids[i],
                                    measureResolution = MEASURE_RESOLUTION,
                                    incidentsDrawing = TRUE,
                                    markExecutionStartTime = TRUE,
                                    measures = "ALL")
    
    ggsave(filename = paste("./graphs/ESWA/medoid",i,".pdf",sep = ""),plot=plot)
  }
}

if (SAVE_PLOT) {
  for (i in 1:length(simSeries)) {
    plot <- plotHelper.simulationTS(simSeries = simSeries,
                                    simulationID = names(simSeries)[[i]],
                                    measureResolution = MEASURE_RESOLUTION,
                                    incidentsDrawing = TRUE,
                                    markExecutionStartTime = TRUE,
                                    measures = "ALL")
    
    ggsave(filename = paste("./graphs/ESWA/simulationProfiles/simulationProfile_",
                            names(simSeries)[[i]],
                            ".pdf",
                            sep = ""),
           plot=plot)
  }
}

