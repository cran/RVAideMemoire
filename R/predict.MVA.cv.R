predict.MVA.cmv <- predict.MVA.cv <- function(object,newdata,conf.level=0.95,type.DA=c("max","all"),
  crit.DA=c("plug-in","predictive","debiased"),...) {
  newdata <- as.matrix(as.data.frame(newdata))
  if (ncol(newdata)==1) {newdata <- t(newdata)}
  if (object$type=="quant") {
    pred <- do.call("cbind",lapply(object$models.list,function(x) predict(x,newdata,ncomp=x$ncomp)))
    pred.mean <- rowMeans(pred)
    conf <- t(apply(pred,1,function(x) t.test(x,conf.level=conf.level)$conf.int))
    res <- data.frame(Mean=pred.mean,CI.inf=conf[,1],CI.sup=conf[,2])
  } else if (object$type=="qual1") {
    type.DA <- match.arg(type.DA)
    if (object$model %in% c("PLS-DA","PPLS-DA")) {
	pred.dummy <- lapply(object$models.list,function(x) predict(x,newdata,ncomp=x$ncomp))
	for (i in 1:length(pred.dummy)) {
	  colnames(pred.dummy[[i]]) <- object$groups
	}
	pred <- do.call("cbind",lapply(pred.dummy,function(x) as.data.frame(colnames(x)[apply(x,1,function(y) which.max(y))])))
	colnames(pred) <- 1:ncol(pred)
	ta <- list()
	for (i in 1:nrow(pred)) {
	  ta[[i]] <- table(factor(unlist(pred[i,]),levels=object$groups))
	}
	res.temp <- as.data.frame(prop.table(do.call("rbind",ta),margin=1))
	if (type.DA=="max") {
	  group <- apply(res.temp,1,function(x) colnames(res.temp)[which.max(x)])
	  proba <- apply(res.temp,1,max)
	  res <- data.frame(Group=group,Proba=proba)
	} else {
	  res <- res.temp
	}
    } else {
	pred <- list()
	for (i in 1:length(object$models.list)) {	
	  pred[[i]] <- predict(object$models.list[[i]],newdata,method=crit.DA)$posterior
	}
	res.temp <- matrix(0,nrow=nrow(pred[[1]]),ncol=ncol(pred[[1]]),dimnames=list(1:nrow(pred[[1]]),object$models.list[[1]]$lev))
	for (i in 1:nrow(res.temp)) {
	  res.temp[i,] <- colMeans(do.call("rbind",lapply(pred,function(x) x[i,])))
	}
	res.temp <- as.data.frame(res.temp)
	colnames(res.temp) <- unlist(lapply(strsplit(colnames(res.temp),"Y."),function(x) x[2]))
	if (type.DA=="max") {
	  group <- apply(res.temp,1,function(x) colnames(res.temp)[which.max(x)])
	  proba <- apply(res.temp,1,max)
	  res <- data.frame(Group=group,Proba=proba)
	} else {
	  res <- res.temp
	}
    }
  } else {
    pred1 <- lapply(object$models1.list,function(x) predict(x,newdata,type="scores"))
    pred2 <- list()
    type.DA <- match.arg(type.DA)
    for (i in 1:length(pred1)) {
	pred2[[i]] <- predict(object$models2.list[[i]],pred1[[i]],method=crit.DA)$posterior
    }
    res.temp <- matrix(0,nrow=nrow(pred2[[1]]),ncol=ncol(pred2[[1]]),dimnames=list(1:nrow(pred2[[1]]),object$models2.list[[1]]$lev))
    for (i in 1:nrow(res.temp)) {
	res.temp[i,] <- colMeans(do.call("rbind",lapply(pred2,function(x) x[i,])))
    }
    res.temp <- as.data.frame(res.temp)
    if (type.DA=="max") {
	group <- apply(res.temp,1,function(x) colnames(res.temp)[which.max(x)])
	proba <- apply(res.temp,1,max)
	res <- data.frame(Group=group,Proba=proba)
    } else {
	res <- res.temp
    }
  }
  rownames(res) <- rownames(newdata)
  return(res)
}
