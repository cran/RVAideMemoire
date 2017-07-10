# ade4 : discrimin.coa

print.CDA.cv <- function(x,...) {
  cat("\n        Cross validation\n\n")
  cat("Model: CDA\n")
  cat(paste0(x$k,"-fold validation\n"))
  if (x$repet>1) {cat(paste0("Validation repeated ",x$repet," times\n"))}
  cat(paste0(x$repet*x$k," submodels generated (",x$ncomp," components)\n"))
  cat(paste0("\nClassification criterion:"),ifelse(x$method=="mahalanobis","Mahalanobis","Euclidian"),"distance\n")
  cat("\n")
  cat(paste0("Mean (standard error) classification error rate (%): ",signif(100*mean(x$NMC),3)," (",
    signif(100*se(x$NMC),2),")\n"))
  cat("\n")
}

CDA.cv <- function(X,Y,repet=10,k=7,ncomp=NULL,method=c("mahalanobis","euclidian")) {
  method <- match.arg(method)
  if (!is.data.frame(X)) {X <- as.data.frame(X)}
  if (!is.factor(Y)) {Y <- factor(Y)}
  if (is.null(ncomp)) {ncomp <- nlevels(Y)-1}
  whole.set <- as.data.frame(cbind(Y,X))
  rownames(whole.set) <- 1:nrow(whole.set)
  k2 <- findk(whole.set,Y,k)
  if (k2!=k) {warning(paste("'k' re-set to",k2))}
  k <- k2
  col.Y <- 1
  col.X <- 2:ncol(whole.set)
  test.sets.list.repet <- list()
  length(test.sets.list.repet) <- repet
  test.sets.list.repet <- lapply(test.sets.list.repet,function(x) {splitf(whole.set,Y,k)})
  test.sets.repet.Y <- lapply(test.sets.list.repet,function(x) {lapply(x,function(y) {Y[as.numeric(rownames(y))]})})
  models.list <- list()
  length(models.list) <- repet*k
  names(models.list) <- paste(rep(1:repet,each=k),rep(1:k,repet),sep=":")
  NMC <- numeric(repet)
  for (i in 1:repet) {
    test.sets.list.k <- test.sets.list.repet[[i]]
    pred <- character(length(Y))
    for (j in 1:k) {
	test.set <- test.sets.list.k[[j]]
	test.set.X <- as.data.frame(test.set[,col.X])
	train.set <- whole.set[-as.numeric(rownames(test.set)),]
	train.set.Y <- Y[-as.numeric(rownames(test.set))]
	train.set.X <- as.data.frame(train.set[,col.X])
	model.k <- ade4::discrimin.coa(train.set.X,train.set.Y,scannf=FALSE,nf=ncomp)
	model.k$df <- train.set.X
	model.k$fac <- train.set.Y
	models.list[[i*k-(k-j)]] <- model.k
	pred.lev <- as.character(predict(model.k,newdata=test.set.X,method=method,dim=ncomp))
	pred[as.numeric(rownames(test.set))] <- pred.lev
    }
    pred.correct <- pred==as.character(Y)
    rate <- 1-sum(pred.correct)/length(pred.correct)
    NMC[i] <- rate
  }
  res <- list(repet=repet,k=k,ncomp=ncomp,method=method,groups=levels(Y),models.list=models.list,NMC=NMC)
  class(res) <- c("list","CDA.cv")
  return(res)
}
