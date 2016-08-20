pairwise.CDA.test <- function(X,fact,ncomp=NULL,p.method="fdr",...) {
  if (nrow(X)!=length(fact)) {
    stop(paste("'",deparse(substitute(X)),"' and '",deparse(substitute(fact)),"' lengths differ",sep=""))
  }
  if (!is.factor(fact)) {fact <- factor(fact)}
  if (is.null(ncomp)) {ncomp <- nlevels(fact)-1}
  data.name <- paste0(deparse(substitute(X))," and ",deparse(substitute(Y)),"\n",ncomp," component",ifelse(ncomp>1,"s",""))
  method <- if (ncomp>1) {
    "MANOVAs"
  } else {
    "ANOVAs"
  }
  fun.p <- function(i,j) {
    X2 <- X[as.numeric(fact) %in% c(i,j),]
    fact2 <- droplevels(fact[as.numeric(fact) %in% c(i,j)])
    test <- CDA.test(X2,fact2,ncomp=ncomp,...)
    res <- if (ncomp>1) {
	test$stats[nrow(test$stats)-1,ncol(test$stats)]
    } else {
	test[nrow(test)-1,ncol(test)]
    }
    res
  }
  multcomp <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
  result <- list(method=method,data.name=data.name,p.value=multcomp,p.adjust.method=p.method)
  class(result) <- "pairwise.htest"
  return(result)
}
