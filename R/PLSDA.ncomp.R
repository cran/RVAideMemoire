PLSDA.ncomp <- function(X,Y,pred.method=c("mahalanobis.dist","centroids.dist","max.dist"),M=10,nrep=10) {
  if (packageVersion("mixOmics")<"4.1.3") {
    stop(paste("you must update 'mixOmics' to version >= 4.1.3 (actual: ",
	packageVersion("mixOmics"),")",sep=""))
  }
  if (length(pred.method)!=1) {pred.method <- "mahalanobis.dist"}
  if (!pred.method%in%c("max.dist","centroids.dist","mahalanobis.dist")) {stop("distance criterion not recognized")}
  ncolX <- ncol(X)
  model0 <- mixOmics::plsda(X,Y,ncomp=ncolX)
  test <- try(mixOmics::valid(model0,method=pred.method,validation="Mfold",folds=M),silent=TRUE)
  if ("try-error"%in%class(test)) {ncolX <- ncolX-1}
  model <- mixOmics::plsda(X,Y,ncomp=ncolX)
  val <- DA.valid(model,method="Mfold",crit.plsda=pred.method,M=M,nrep=nrep)
  vars <- DA.var(model)
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  par(mar=c(5,4,2,5))
  plot(1:ncolX,val$tab[,1],type="l",col="red",axes=FALSE,xlab="Number of components",ylab="",ylim=c(0,100),
    lwd=2,xaxs="i",yaxs="i")
  axis(1,at=1:ncolX,labels=1:ncolX,cex.axis=0.7)
  box()
  axis(2,col.axis="red",col="red")
  mtext(paste("Classification error rate (%); M-fold (",val$M,"groups); ",val$nrep," repetitions",sep=""),
    side=2,line=2.5,col="red",las=0)
  nlev <- nlevels(Y)
  abline(v=nlev-1,lty=3)
  segments(0,val$tab[nlev-1,1],nlev-1,val$tab[nlev-1,1],lty=3,col="red")
  lines(1:ncolX,vars[,"Cumulative"],type="l",col="blue",lwd=2)
  axis(4,col.axis="blue",col="blue")
  mtext("Proportion of intergroup variance explained (%)",side=4,line=2.5,col="blue",las=0)
  segments(nlev-1,vars[nlev-1,"Cumulative"],ncolX,vars[nlev-1,"Cumulative"],lty=3,col="blue")
}
