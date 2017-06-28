clusteringHelpers.StableKmeans = function (data,K) {
  
  mat <- as.matrix(data)
  
  clusterObjInit <- hclust(dist(mat, method = "euclidean"),"average")
  initial <- tapply(mat, list(rep(cutree(clusterObjInit,K),ncol(mat)),col(mat)),
                    function(x) mean(x, na.rm=TRUE))
  if(length(dup <- which(duplicated(initial)))>0) {
    for(dupi in dup) 
      initial[dupi,] <- initial[dupi,] + jitter(initial[dupi,])
  }
  dimnames(initial) <- list(NULL,dimnames(mat)[[2]])
  sol <- kmeans(mat,initial)
  
  return(sol)
}

# function to find medoid in cluster i
clusteringHelpers.medoid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}

##
# clusteringResults: NAMED vector (names are ids)
##
clusteringHelpers.belongToSameCluster <- function(clusteringResults,id1,id2) {
  clusteringResults[[id1]] == clusteringResults[[id2]]
}