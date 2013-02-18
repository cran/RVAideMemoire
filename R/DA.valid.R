DA.valid <-
function(model,method=c("loo","Mfold"),crit.lda=c("plug-in","predictive","debiased"),M=10,nrep=20) {
  if (class(model)!="lda") {stop("model not recognized")}
  if (length(method)!=1) {method <- "Mfold"}
  if (!method%in%c("Mfold","loo")) {stop("method not recognized")}
  result <- list(model=class(model),method=method)
  if (class(model)=="lda") {
    form <- LDA.format(model)
    X <- form$x
    grouping <- form$grouping
    ncomp <- ncol(model$scaling)
    if (length(crit.lda)!=1) {crit.lda <- "plug-in"}
    if (!crit.lda%in%c("plug-in","predictive","debiased")) {stop("estimation method not recognized")}
    result$crit.lda <- crit.lda
    if (method=="loo") {
	pred <- matrix(0,nrow=nrow(X),ncol=ncomp)
	for (i in 1:nrow(X)) {
	  model <- lda(X[-i,],grouping[-i])
	  for (j in 1:ncomp) {
	    p <- predict(model,X[i,],dimen=j,method=crit.lda)$class
	    if (p==grouping[i]) {pred[i,j] <- 1}
	  }
	}
	pc <- apply(pred,2,function(x) 100-100*sum(x)/length(x))
	tab <- data.frame("Error rate (%)"=pc,row.names=paste("Axis",1:ncomp),check.names=FALSE)
	result$tab <- tab
    } else if (method=="Mfold") {
	tab.temp <- matrix(0,nrow=ncomp,ncol=nrep,dimnames=list(paste("Axis",1:ncomp),paste("Run",1:nrep,sep="")))
	if (nrep>1) {pb <- txtProgressBar(min=0,max=100,initial=0,style=3)}
	for (i in 1:nrep) {
	  ind.tot <- sample(1:nrow(X))
	  ind.tochoice <- ind.tot
	  nb.ind <- round(length(ind.tot)/M,0)
	  pred <- matrix(0,nrow=M,ncol=ncomp)
	  for (j in 1:M) {
	    samp <- if (j<M) {
		ind.tochoice[1:nb.ind]
	    } else {
		ind.tochoice
	    }
	    ind.tochoice <- ind.tochoice[-(1:nb.ind)]
	    train <- X[-samp,]
	    test <- X[samp,]
	    model <- lda(train,grouping[-samp])
	    for (k in 1:ncomp) {
		p <- predict(model,test,dimen=k,method=crit.lda)$class
		pred[j,k] <- sum(p==grouping[samp])/length(samp)
	    }
	  }
	  tab.temp[,i] <- apply(pred,2,function(x) 100-100*sum(x)/length(x))
	  if (nrep>1) {setTxtProgressBar(pb,round(i*100/nrep,0))}
	}
	tab <- data.frame("Error rate (%)"=rowMeans(tab.temp),row.names=paste("Axis",1:ncomp),check.names=FALSE)
	result$M <- M
	result$nrep <- nrep
	result$tab <- tab
    }
  }
  class(result) <- "DA.valid"
  return(result)
}

print.DA.valid <-
function(x,digits=4,...) {
  model <- "LDA"
  cat(paste("\n\tCross-validation on a ",model," model\n\n",sep=""))
  m <- if (x$method=="loo") {
    "Leave-one-out"
  } else {
    paste("M-fold (",x$M," groups)",sep="")
  }
  cat(paste("Method: ",m,"\n",sep=""))
  if (x$method=="Mfold") {
    cat(paste("Repetitions: ",x$nrep,"\n",sep=""))
  }
  crit <- x$crit.lda
  cat(paste("Parameter estimation method: ",crit,"\n\n",sep=""))
  print(x$tab,digits=digits)
  cat("\n")
}
