predict.CDA.cv <- function(object,newdata,method=c("mahalanobis","euclidian"),...) {
  method <- match.arg(method)
  newdata <- as.data.frame(newdata)
  if (ncol(newdata)==1) {newdata <- t(newdata)}
  pred <- do.call("cbind",lapply(object$models.list,function(x) as.character(predict(x,newdata,method=method))))
  ta <- list()
  for (i in 1:nrow(pred)) {ta[[i]] <- table(unlist(pred[i,]))}
  group <- factor(unlist(lapply(ta,function(x) names(x)[which.max(x)[1]])))
  proba <- unlist(lapply(ta,function(x) x[which.max(x)[1]]/sum(x)))
  res <- data.frame(Group=group,Proba=proba)
  rownames(res) <- rownames(newdata)
  return(res)
}
