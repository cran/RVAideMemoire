recover.data.clm <- function(object,...) {
  fcall <- object$call
  recover.data(fcall,delete.response(terms(object)),object$na.action,...)
}

lsm.basis.clm <- function(object,trms,xlev,grid) {
  if (object$threshold!="flexible") {
    stop("lsmeans deals only with models based on flexible thresholds")
  }
  contrasts <- object$contrasts
  m <- model.frame(trms,grid,na.action=na.pass,xlev=xlev)
  X <- model.matrix(trms,m,contrasts.arg=contrasts)
  xint <- match("(Intercept)",colnames(X),nomatch=0L)
  if (xint>0L) {X <- X[,-xint,drop=FALSE]}
  bhat <- c(object$beta,object$alpha)
  V <- vcov(object)
  k <- length(object$alpha)
  j <- matrix(1,nrow=k,ncol=1)
  J <- matrix(1,nrow=nrow(X),ncol=1)
  X <- cbind(kronecker(-j,X),kronecker(diag(1,k),J))
  link <- as.character(object$info$link)
  misc <- list(ylevs=list(cut=names(object$alpha)),tran=link,inv.lbl="cumprob")
  nbasis <- matrix(NA)
  dffun <- function(...) NA
  list(X=X,bhat=bhat,nbasis=nbasis,V=V,dffun=dffun,dfargs=list(),misc=misc)
}
