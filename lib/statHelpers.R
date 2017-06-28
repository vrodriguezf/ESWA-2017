##
# Penrose: Multivariate distance
# See http://people.stat.sc.edu/habing/courses/530rF01.html
##
statHelper.penroseDistance <- function(matrixlist) {
  p <- ncol(matrixlist[[1]]) #Number of Dimensions
  
  #Matrix of means (each row is the mean of a data instance)
  xbarmatrix <- do.call(what = rbind,args = lapply(matrixlist,function (x) apply(x,2,mean)))
  
  #Variance diagonal matrix from covariance matrix
  C_numerator <- matrixlist %>% 
    map(function(x) cov(x)*(nrow(x)-1)) %>% 
    reduce(.f = function(x1,x2) x1+x2,.init = matrix(0,nrow=p,ncol=p))
  
  C_denominator <- (matrixlist %>% map(length) %>% unlist %>% sum) - length(matrixlist)
  C <- C_numerator/C_denominator
  variancediagonalmatrix <- diag(diag(C))
  
  penroseMatrix <- matrix(0,nrow=length(matrixlist),ncol = length(matrixlist))
  for (i in 1:length(matrixlist)) {
    for (j in 1:length(matrixlist)) {
      penroseMatrix[i,j] <- mahalanobis(xbarmatrix[i,],xbarmatrix[j,],variancediagonalmatrix)/p
    }
  }
  
  return(as.dist(penroseMatrix))
}

##
#
##
statHelper.mahalanobisDistance <- function(matrixlist) {
  p <- ncol(matrixlist[[1]]) #Number of Dimensions
  
  #Matrix of means (each row is the mean of a data instance)
  xbarmatrix <- do.call(what = rbind,args = lapply(matrixlist,function (x) apply(x,2,mean)))
  
  #Variance diagonal matrix from covariance matrix
  C_numerator <- matrixlist %>% 
    map(function(x) cov(x)*(nrow(x)-1)) %>% 
    reduce(.f = function(x1,x2) x1+x2,.init = matrix(0,nrow=p,ncol=p))
  
  C_denominator <- (matrixlist %>% map(length) %>% unlist %>% sum) - length(matrixlist)
  C <- C_numerator/C_denominator
  
  mahalanobisMatrix <- matrix(0,nrow=length(matrixlist),ncol = length(matrixlist))
  for (i in 1:length(matrixlist)) {
    for (j in 1:length(matrixlist)) {
      mahalanobisMatrix[i,j] <- mahalanobis(xbarmatrix[i,],xbarmatrix[j,],C)
    }
  }
  
  return(as.dist(mahalanobisMatrix))
}