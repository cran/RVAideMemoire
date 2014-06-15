recover.data.clmm <- function(object,...) {
  fcall <- object$call
  recover.data(fcall,delete.response(terms(object)),object$na.action,...)
}

lsm.basis.clmm <- function(object,trms,xlev,grid) {
  if (object$threshold!="flexible") {
    stop("lsmeans deals only with models based on flexible thresholds")
  }
  contrasts <- object$contrasts
  m <- model.frame(trms,grid,na.action=na.pass,xlev=xlev)
  X <- model.matrix(trms,m,contrasts.arg=contrasts)
  xint <- match("(Intercept)",colnames(X),nomatch=0L)
  if (xint>0L) {X <- X[,-xint,drop=FALSE]}
  bhat <- c(object$beta,object$alpha)
  H <- object$Hessian
  if (any(apply(object$Hessian,1,function(x) all(x==0)))) {
    H <- H[names(coef(object)),names(coef(object))]
    object$Hessian <- H
  }
  V <- vcov(object)
  n.rand <- length(object$gfList)
  names.rand <- paste0("ST",1:n.rand)
  V <- V[-which(rownames(V)%in%names.rand),-which(colnames(V)%in%names.rand)]
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

