pairwise.MVA.test <- function(X,fact,p.method="fdr",cmv=FALSE,ncomp=8,kout=7,kinn=6,
  model=c("PLS-DA","PPLS-DA","LDA","QDA","PLS-DA/LDA","PLS-DA/QDA","PPLS-DA/LDA","PPLS-DA/QDA"),
  nperm=999,progress=TRUE,...) {
  model <- match.arg(model)
  if (model %in% c("LDA","QDA") & cmv) {stop("LDA and QDA alone only if cmv = FALSE")}
  if (nrow(X)!=length(fact)) {
    stop(paste("'",deparse(substitute(X)),"' and '",deparse(substitute(fact)),"' lengths differ",sep=""))
  }
  if (!is.factor(fact)) {fact <- factor(fact)}
  data.name <- paste0(deparse(substitute(X))," and ",deparse(substitute(Y)),"\nModel: ",model,
    "\n",ncomp," components",ifelse(cmv," maximum",""),"\n",nperm," permutations")
  method <- if (!cmv) {
    "permutation tests based on cross-validation"
  } else {
    "permutation tests based on cross model validation"
  }
  fun.p <- function(i,j) {
    X2 <- X[as.numeric(fact)%in%c(i,j),]
    fact2 <- droplevels(fact[as.numeric(fact)%in%c(i,j)])
    MVA.test(X2,fact2,cmv=cmv,ncomp=ncomp,kout=kout,kinn=kinn,model=model,nperm=nperm,
	progress=progress,...)$p.value
  }
  multcomp <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
  result <- list(method=method,data.name=data.name,p.value=multcomp,p.adjust.method=p.method,
    permutations=nperm)
  class(result) <- "pairwise.htest"
  return(result)
}
