DA.valid <-
function(model,method=c("Mfold","loo"),crit.lda=c("plug-in","predictive","debiased"),
  crit.plsda=c("mahalanobis.dist","centroids.dist","max.dist"),
  crit.cda=c("mahalanobis","euclidian"),M=10,nrep=20) {
  if (!any(class(model)%in%c("lda","plsda","coadisc"))) {stop("model not recognized")}
  method <- match.arg(method)
  if (method=="Mfold" & M<=1) {stop("invalid number of folds (must be > 1)")}
  result <- list(model=class(model)[1],method=method)
  if (any(class(model)=="lda")) {
    form <- LDA.format(model)
    X <- form$x
    grouping <- form$grouping
    ncomp <- ncol(model$scaling)
    crit.lda <- match.arg(crit.lda)
    result$crit.lda <- crit.lda
    if (method=="loo") {
	pred <- matrix(0,nrow=nrow(X),ncol=ncomp)
	for (i in 1:nrow(X)) {
	  model <- MASS::lda(X[-i,],grouping[-i],prior=model$prior)
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
	    model <- MASS::lda(train,grouping[-samp],prior=model$prior)
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
  } else if (any(class(model)=="plsda")) {
    if (packageVersion("mixOmics")<"5.0.2") {
	stop(paste("you must update 'mixOmics' to version >= 5.0.2 (actual: ",
	  packageVersion("mixOmics"),")",sep=""))
    }
    ncomp <- model$ncomp
    crit.plsda <- match.arg(crit.plsda)
    result$crit.plsda <- crit.plsda
    tab.temp <- data.frame(Run1=integer(ncomp),row.names=paste("Axis",1:ncomp))
    if (method=="loo" | (method=="Mfold" & nrep==1)) {
	ok <- FALSE
	while (!ok) {
	  test <- try(mixOmics::perf(model,validation=method,method.predict=crit.plsda,folds=M,progressBar=FALSE),silent=TRUE)
	  if ("try-error"%in%class(test)) {
	    next
	  } else {
	    val <- test
	    ok <- TRUE
	  }
	}
	tab.temp[,1] <- val$error.rate[,crit.plsda]
	if (method=="Mfold") {
	  result$M <- M
	  result$nrep <- nrep
	}
    } else {
	for (i in 2:nrep) {
	  tab.temp <- cbind(tab.temp,integer(ncomp))
	  colnames(tab.temp)[i] <- paste("Run",i,sep="")
	}
	pb <- txtProgressBar(min=0,max=100,initial=0,style=3)
	for (i in 1:nrep) {
	  ok <- FALSE
	  while (!ok) {
	    test <- try(mixOmics::perf(model,validation="Mfold",method.predict=crit.plsda,folds=M,progressBar=FALSE),silent=TRUE)
	    if ("try-error"%in%class(test)) {
		next
	    } else {
		val <- test
		ok <- TRUE
	    }
	  }
	  tab.temp[,i] <- val$error.rate[,crit.plsda]
	  setTxtProgressBar(pb,round(i*100/nrep,0))
	}
	cat("\n")
	result$M <- M
	result$nrep <- nrep
    }
    tab <- data.frame("Error rate (%)"=integer(ncomp),row.names=paste("Axis",1:ncomp),check.names=FALSE)
    tab[,1] <- 100*apply(tab.temp,1,mean)
    result$tab <- tab
  } else   if (any(class(model)=="coadisc")) {
    X <- eval.parent(model$call$df)
    grouping <- eval.parent(model$call$fac)
    ncomp <- model$nf
    crit.cda <- match.arg(crit.cda)
    result$crit.cda <- crit.cda
    if (method=="loo") {
	pred <- matrix(0,nrow=nrow(X),ncol=ncomp)
	for (i in 1:nrow(X)) {
	  model <- ade4::discrimin.coa(X[-i,],grouping[-i],scannf=FALSE,nf=ncomp)
	  for (j in 1:ncomp) {
	    p <- predict(model,X[i,],dim=j,method=crit.cda)
	    if (as.character(p)==as.character(grouping[i])) {pred[i,j] <- 1}
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
	    model <- ade4::discrimin.coa(train,factor(grouping[-samp]),scannf=FALSE,nf=ncomp)
	    for (k in 1:ncomp) {
		p <- predict(model,test,dim=k,method=crit.cda)
		pred[j,k] <- sum(as.character(p)==as.character(grouping[samp]))/length(samp)
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
  model <- if (x$model=="lda") {
    "LDA"
  } else if (x$model=="plsda") {
    "PLS-DA"
  } else {
    "CDA"
  }
  cat(paste("\n\tCross-validation on a ",model,"\n\n",sep=""))
  m <- if (x$method=="loo") {
    "Leave-one-out"
  } else {
    paste("M-fold (",x$M," groups)",sep="")
  }
  cat(paste("Method: ",m,"\n",sep=""))
  if (x$method=="Mfold") {
    cat(paste("Repetitions: ",x$nrep,"\n",sep=""))
  }
  crit <- if (x$model=="lda") {
    x$crit.lda
  } else if (x$model=="plsda") {
    if (x$crit.plsda=="max.dist") {
	"maximum distance"
    } else if (x$crit.plsda=="centroids.dist") {
	"centroids distance"
    } else {
	"Mahalanobis distance"
    }
  } else {
    if (x$crit.cda=="euclidian") {
	"euclidian distance"
    } else {
	"Mahalanobis distance"
    }
  }
  cat(paste(ifelse(x$model=="lda","Parameter estimation method: ","Distance criterion: "),crit,"\n\n",sep=""))
  print(x$tab,digits=digits)
  cat("\n")
}

