PLSDA.test <-
function(model,pred.method=c("mahalanobis.dist","centroids.dist","max.dist"),M=10,nperm=999) {
  if (class(model)!="plsda") {stop("model not recognized")}
  if (length(pred.method)!=1) {pred.method <- "mahalanobis.dist"}
  if (!pred.method%in%c("max.dist","centroids.dist","mahalanobis.dist")) {stop("distance criterion not recognized")}
  data.name <- paste(deparse(substitute(model)),"\nMethod: Mfold (",M," groups)\nDistance criterion: ",
    ifelse(pred.method=="mahalanobis.dist","mahalanobis distance",ifelse(pred.method=="centroids.dist","centroids distance",
    "maximum distance")),"\nAxes: ",model$ncomp,"\n",nperm," permutations",sep="")
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
  n.by.lev <- table(Y)
  n.min <- min(n.by.lev)
  Y.n.min <- rep(levels(Y),each=n.min)
  ncomp <- model$ncomp
  pc.ref.all <- integer(30)
  for (i in 1:30) {
    X.val <- matrix(0,nrow=nlevels(Y)*n.min,ncol=ncol(X))
    for (j in 1:nlevels(Y)) {
	X.j <- X[as.numeric(Y)==j,]
	X.val[(j*n.min-(n.min-1)):(j*n.min),] <- X.j[sample(1:n.by.lev[j],n.min),]
    }
    model.ref <- plsda(X.val,Y.n.min,ncomp=ncomp)
    val <- valid(model.ref,method=pred.method,validation="Mfold",folds=M)
    pc.ref.all[i] <- 100*val[nrow(val),1]
  }
  pc.ref <- mean(pc.ref.all)
  null.value <- paste(round(pc.ref,1),"%",sep="")
  names(null.value) <- "classification error rate"
  estimate <- round(pc.ref,1)
  names(estimate) <- "classification error rate (%)"
  pb <- txtProgressBar(min=0,max=100,initial=0,style=3)
  pc.perm <- integer(nperm)
  for (i in 1:nperm) {
    X.perm <- matrix(0,nrow=nlevels(Y)*n.min,ncol=ncol(X))
    for (j in 1:nlevels(Y)) {
	X.j <- X[as.numeric(Y)==j,]
	X.perm[(j*n.min-(n.min-1)):(j*n.min),] <- X.j[sample(1:n.by.lev[j],n.min),]
    }
    Y.perm <- sample(Y.n.min)
    model.perm <- plsda(X.perm,Y.perm,ncomp=ncomp)
    val.perm <- valid(model.perm,method=pred.method,validation="Mfold",folds=M)
    pc.perm[i] <- 100*val.perm[nrow(val.perm),1]
    setTxtProgressBar(pb,round(i*100/nperm,0))
  }
  cat("\n")
  pval <- 1-length(which(pc.perm>=pc.ref))/(nperm+1)
  result <- list(method="Permutational test for the discriminant ability of the factor in PLS-DA",data.name=data.name,
    p.value=pval,alternative="greater",null.value=null.value,estimate=estimate,pred.method=pred.method,M=M,
    permutations=nperm)
  class(result) <- "htest"
  return(result)
}
