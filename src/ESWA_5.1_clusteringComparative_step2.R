#
# Clustering comparative for step 2 of the proposed methodology
# Compare the best clustering solution found using our methodology against
# the solutions found by using direct clustering by multivariate time series distances
# (Mean DTW, Mean Frechet, Penrose, Mahalanobis)
#
if (!exists("PIPELINE_CALL") || PIPELINE_CALL == FALSE) {
  library('ProjectTemplate')
  load.project()
  SAVE_PLOT = FALSE
  SAVE_OUTPUT = TRUE
  COMPUTE_DISSIMILARITIES <- TRUE
}

#Format data
simSeriesMatrixList <- lapply(simSeries,function (x) do.call(what = cbind,args = x))

# Test different multivariate-time series clustering techniques
if (COMPUTE_DISSIMILARITIES) {
  distanceMatrices <- list(
    penrose= statHelper.penroseDistance(simSeriesMatrixList),
    mahalanobis = statHelper.mahalanobisDistance(simSeriesMatrixList),
    meanDTWARP = timeSeriesHelper.multivariate_mean(simSeriesMatrixList,"DTWARP"),
    meanFRECHET = timeSeriesHelper.multivariate_mean(simSeriesMatrixList,"FRECHET")
  )
}

step2ComparativeValidationResults <- lapply(distanceMatrices, function (x) {
  temp <- ESWA_helper.clusteringValidation(
    dissMatrix = x,
    clustMethods = list(c(name="PAM",command=pam,params=NULL)),
    K=3:8,
    returnAllResults = TRUE
  )
  
  temp <- lapply(temp, function (simClust) {
    simClust$clustering <- simClust$clustering %>% setNames(names(simSeries))
    simClust
  })
  
  #temp$bestClust$clustering<- temp$bestClust$clustering %>% setNames(names(simSeries))
  
  return(temp)
})

# Evaluate the assignation clusters using the human ground truth
humanGroundTruth <- ESWA_helper.loadHumanGroundTruth()

step2ComparativeEvaluationResults <- lapply(step2ComparativeValidationResults, function (case) {
  lapply(case, function (x) {
    ESWA_helper.clusteringEvaluation(
      clusteringResults = x$clustering,
      humanGroundTruth = humanGroundTruth)
  })
})

step2ComparativeEvaluationResults <- do.call(rbind.data.frame,step2ComparativeEvaluationResults) %>% t

# if (exists("step2MethodologyEvaluationResults") && is.data.frame(step2MethodologyEvaluationResults))
#   step2ComparativeEvaluationResults <- do.call(rbind.data.frame,step2ComparativeEvaluationResults) %>% rbind(step2MethodologyEvaluationResults)

