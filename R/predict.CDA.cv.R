predict.CDA.cv <- function(object,newdata,type=c("max","all"),method=c("mahalanobis","euclidian"),...) {
  type <- match.arg(type)
  method <- match.arg(method)
  newdata <- as.data.frame(newdata)
  if (ncol(newdata)==1) {newdata <- t(newdata)}
  pred <- do.call("cbind",lapply(object$models.list,function(x) as.character(predict(x,newdata,method=method))))
  ta <- list()
  for (i in 1:nrow(pred)) {
    ta[[i]] <- table(factor(unlist(pred[i,]),levels=object$groups))
  }
  res.temp <- as.data.frame(prop.table(do.call("rbind",ta),margin=1))
  if (type=="max") {
    group <- apply(res.temp,1,function(x) colnames(res.temp)[which.max(x)])
    proba <- apply(res.temp,1,max)
    res <- data.frame(Group=group,Proba=proba)
  } else {
    res <- res.temp
  }
  rownames(res) <- rownames(newdata)
  return(res)
}
