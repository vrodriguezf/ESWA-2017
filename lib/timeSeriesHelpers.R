##
# Mean distance for multivariate time series averaging the distance matrices
# for each of the data dimensions (uses TSClust package to definethe diss method)
# Input: List of multivariate data (matrix nxp) (one time series per column)
##
timeSeriesHelper.multivariate_mean <- function (matrixList,dissMethod) {
  distMatrices <- lapply(1:ncol(matrixList[[1]]), function (tsIndex) {
    matrixList %>% 
      lapply(function (data) data[,tsIndex]) %>%
      diss(METHOD=dissMethod)
  })
    
  return(Reduce("+", distMatrices) / length(distMatrices))
}