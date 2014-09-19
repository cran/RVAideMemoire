DA.var <- function(model) {
  warning("DA.var() will be deprecated soon, use the more generic MVA.synt()")
  if (!class(model)%in%c("lda","plsda")) {stop("model not recognized")}
  if (class(model)=="lda") {
    model <- LDA.format(model)
    Y <- model$grouping
    if (nlevels(Y)==2) {stop("only 2 levels, hence 1 axis -> 100% of variance explained by this axis")}
    ncomp <- ncol(model$li)
    ncomp.tot <- ncomp
    coord <- model$li
  } else {
    if (packageVersion("mixOmics")<"5.0.2") {
	stop(paste("you must update 'mixOmics' to version >= 5.0.2 (actual: ",
	  packageVersion("mixOmics"),")",sep=""))
    }
    X <- model$X
    Y <- character(nrow(X))
    for (i in 1:length(Y)) {
	if (any(model$ind.mat[i,]!=0)) {
	  Y[i] <- model$names$Y[which(model$ind.mat[i,]==1)]
	} else {
	  Y[i] <- NA
	}
    }
    Y <- factor(Y)
    if (any(is.na(Y))) {
	X <- X[-which(is.na(Y)),]
	Y <- na.omit(Y)
    }
    ncomp <- model$ncomp
    ncomp.tot <- ncol(X)
    model2 <- mixOmics::plsda(X,Y,ncomp=ncomp.tot)
    coord <- model2$variates$X
  }
  weights <- table(Y)
  means <- aggregate(coord,list(Y=Y),mean)[,-1]
  col.means <- matrix(0,ncol=ncomp.tot,nrow=1,dimnames=list(1,paste("comp",1:ncomp.tot,sep="")))
  for (i in 1:ncomp.tot) {
    col.means[1,i] <- sum(means[,i]*weights)/sum(weights)
  }
  vars <- matrix(0,ncol=ncomp.tot,nrow=1,dimnames=list(1,paste("comp",1:ncomp.tot,sep="")))
  for (i in 1:ncomp.tot) {
    vars[1,i] <- sum((means[,i]-col.means[1,i])^2*weights)/sum(weights)
  }
  prop <- round(as.vector(100*vars/sum(vars)),2)
  tab <- data.frame("Proportion (%)"=prop,Cumulative=round(cumsum(prop),2),
    row.names=paste("Comp",1:ncomp.tot,sep=""),check.names=FALSE)
  if (class(model)=="plsda") {
    tab <- tab[1:ncomp,]
  }
  return(tab)
}
