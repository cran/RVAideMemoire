# mixOmics: perf

print.DIABLO.cv <- function(x,...) {
  cat("\n        Cross validation\n\n")
  cat("Model: DIABLO\n")
  if (x$validation=="Mfold") {
    cat(paste0(x$k,"-fold validation\n"))
  } else {
    cat("leave-one-out validation\n")
  }
  if (x$repet>1) {cat(paste0("Validation repeated ",x$repet," times\n"))}
  cat(paste0(x$ncomp[1]," components\n"))
  crit <- if (x$method=="mahalanobis.dist") {
    "Mahalanobis"
  } else if(x$method=="max.dist") {
    "maximum"
  } else {
    "centroids"
  }
  cat(paste0("\nClassification criterion: ",crit," distance\n"))
  cat("\n")
  if (x$repet>1) {
    cat(paste0("Mean (standard error) classification error rate (%): ",signif(100*x$NMC.mean,3)," (",
	signif(100*x$NMC.se,2),")\n"))
  } else {
    cat(paste0("Classification error rate (%): ",signif(100*x$NMC.mean,3),"\n"))
  }
  cat("\n")
}

DIABLO.cv <- function(x,method=c("mahalanobis.dist","max.dist","centroids.dist"),
  validation=c("Mfold","loo"),k=7,repet=10,...) {
  method <- match.arg(method)
  validation <- match.arg(validation)
  k <- findk(x$X[[1]],x$Y,k=k)
  p <- mixOmics::perf(x,dist=method,validation=validation,folds=k,nrepeat=repet,...)
  NMC.mean <- p$WeightedPredict.error.rate
  NMC.mean <- NMC.mean["Overall.ER",ncol(NMC.mean)]
  NMC.se <- if (repet>1) {
    p$WeightedPredict.error.rate.sd["Overall.ER",ncol(p$WeightedPredict.error.rate.sd)]/repet
  } else {NA}
  res <- list(repet=repet,validation=validation,k=k,ncomp=x$ncomp,method=method,NMC.mean=NMC.mean,
    NMC.se=NMC.se)
  class(res) <- "DIABLO.cv"
  return(res)
}
