###
# 
###
helper.extractOIDFromCompleteOIDString <- function (oid)
{
  return(mongo.oid.from.string(str_sub(oid,10,-2)))
}

##
# 
##
helper.getUserName <- function (clientIP,createdAt)
{
  return(
    paste(as.Date(createdAt),str_sub(clientIP,-3),sep = "_")
  )
}

helper.dataFrameDiff <- function(x.1,x.2,...)
{
  x.1p <- do.call("paste", x.1)
  x.2p <- do.call("paste", x.2)
  x.1[! x.1p %in% x.2p, ]
}

"%+%" <- function(...){
  paste0(...,sep="")
}

helper.goodUnlist <- function (x) {
  v <- as.data.frame(t(unlist(x)),stringsAsFactors = FALSE)
  vc <- t(apply(v,1,as.character))
  vn <- t(apply(v,1,as.numeric))
  nas <- which(is.na(vn))
  v[-nas] <- vn[-nas]
  v[nas] <- vc[nas]
  v
}