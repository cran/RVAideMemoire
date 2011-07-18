cox.resid <-
function(model,covar) {
  if (!is.list(covar)) {stop("'covar' is not a list")}
  if (is.null(names(covar))) {
    names(covar)<-paste("covar",1:length(covar),sep="")
    warning("no name for covariables, arbitrary names are given")
  }
  res<-residuals(model,type="martingale")
  mat<-matrix(as.numeric(unlist(covar)),ncol=length(covar),dimnames=list(1:length(covar[[1]]),names(covar)))
  if (ncol(mat)>1) {par(mfrow=c(ceiling(ncol(mat)/2),2))}
  for (i in 1:ncol(mat)) {
    plot(mat[,i],res,xlab=colnames(mat)[i],ylab="Martingale residuals")
    abline(h=0,lty=3,col="grey")
    panel.smooth(mat[,i],res)
  }
}

