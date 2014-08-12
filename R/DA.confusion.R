DA.confusion <-
function(model,train=2/3,crit.lda=c("plug-in","predictive","debiased"),
  crit.plsda=c("mahalanobis.dist","centroids.dist", "max.dist")) {
  if (!class(model)%in%c("lda","plsda")) {
    stop("model not recognized")
  }
  if (train<=0 | train >=1) {stop("'train' must be between > 0 and < 1")}
  result <- list(model=class(model))
  if (class(model)=="lda") {
    if (length(crit.lda)!=1) {crit.lda <- "plug-in"}
    if (!crit.lda%in%c("plug-in","predictive","debiased")) {
	stop("estimation method not recognized")
    }
    result$crit.lda <- crit.lda
    form <- LDA.format(model)
    X <- form$x
    grouping <- form$grouping
    n <- ceiling(train*nrow(X))
    ind <- sample(1:nrow(X),n)
    ech.train <- X[ind,]
    group.train <- grouping[ind]
    group.test <- grouping[-ind]
    mod.train <- MASS::lda(ech.train,group.train,prior=model$prior)
    pred <- predict(mod.train,X[-ind,],method=crit.lda)$class
    pred <- factor(pred,levels=levels(grouping))
    confusion <- table(group.test,pred,dnn=c("Real group","Predicted group"))
    prop.confusion <- 1-sum(diag(confusion))/sum(confusion)
    result$prop.train <- c("used"=n,"total"=nrow(X))
    result$ind.for.train <- ind
    result$predicted <- pred
    result$confusion <- confusion
    result$prop.confusion <- prop.confusion
  }  else if (class(model)=="plsda") {
    if (packageVersion("mixOmics")<"5.0.2") {
	stop(paste("you must update 'mixOmics' to version >= 5.0.2 (actual: ",
	  packageVersion("mixOmics"),")",sep=""))
    }
    if (length(crit.plsda)!=1) {crit.plsda <- "mahalanobis.dist"}
    if (!crit.plsda%in%c("max.dist","centroids.dist","mahalanobis.dist")) {
	stop("distance criterion not recognized")
    }
    result$crit.plsda <- crit.plsda
    X <- model$X
    grouping.indmat <- model$ind.mat
    colnames(grouping.indmat) <- model$names$Y
    grouping <- character(nrow(grouping.indmat))
    for (i in 1:nrow(grouping.indmat)) {
	grouping[i] <- colnames(grouping.indmat)[which(grouping.indmat[i,]==1)]
    }
    grouping <- factor(grouping)
    n <- ceiling(train*nrow(X))
    ind <- sample(1:nrow(X),n)
    ech.train <- X[ind,]
    group.train <- grouping[ind]
    group.test <- grouping[-ind]
    mod.train <- mixOmics::plsda(ech.train,group.train)
    pred.tab <- predict(mod.train,X[-ind,],method=crit.plsda)$class[[1]]
    pred <- levels(grouping)[pred.tab[,ncol(pred.tab)]]
    pred <- factor(pred,levels=levels(grouping))
    confusion <- table(group.test,pred,dnn=c("Real group","Predicted group"))
    prop.confusion <- 1-sum(diag(confusion))/sum(confusion)
    result$prop.train <- c("used"=n,"total"=nrow(X))
    result$ind.for.train <- ind
    result$predicted <- pred
    result$confusion <- confusion
    result$prop.confusion <- prop.confusion
  }
  class(result) <- "DA.confusion"
  return(result)
}

print.DA.confusion <- function(x,...) {
  model <- ifelse(x$model=="lda","LDA","PLS-DA")
  cat(paste("\n\tClassification error rate of a ",model," model\n\n",sep=""))
  crit <- if (x$model=="lda") {
    x$crit.lda
  } else {
    if (x$crit.plsda=="max.dist") {
	"maximum distance"
    } else if (x$crit.plsda=="centroids.dist") {
	"centroids distance"
    } else {
	"mahalanobis distance"
    }
  }
  cat(paste(ifelse(x$model=="lda","Parameter estimation method: ","Distance criterion: "),crit,"\n",sep=""))
  cat(paste(x$prop.train[1]," individuals used for training (among ",x$prop.train[2],
    ", proportion = ",round(100*x$prop.train[1]/x$prop.train[2],1),"%)\n",sep=""))
  cat(paste("Classification error rate: ",round(100*x$prop.confusion,1),"%\n\n",sep=""))
  cat("Confusion matrix:\n")
  print(x$confusion)
  cat("\n")
}
