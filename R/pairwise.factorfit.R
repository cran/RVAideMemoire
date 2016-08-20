## vegan: envfit

pairwise.factorfit <- function(ord,fact,xax=1,yax=2,nperm=999,p.method="fdr",...) {
  data.name <- paste0(deparse(substitute(ord))," by ",deparse(substitute(fact)),"\n",
    nperm," permutations")
  call <- match.call()
  if (!"scaling" %in% names(call)) {call$scaling <- 1}
  X.0 <- MVA.scores(ord,xax=xax,yax=yax,...)
  if ("COA.vegan" %in% class(MVA.ident(ord)) | "CCA.vegan" %in% class(MVA.ident(ord))) {
    X <- X.0$coord[X.0$set=="rows",]
  } else {
    X <- X.0$coord
  }
  fact <- droplevels(factor(fact))
  method <- "factor fitting to an ordination"
  fun.p <- function(i,j) {
    X2 <- X[as.numeric(fact) %in% c(i,j),]
    fact2 <- droplevels(fact[as.numeric(fact) %in% c(i,j)])
    fit <- vegan::envfit(X2~fact2,permutations=nperm)
    fit$factors$pvals
  }
  multcomp <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
  result <- list(method=method,data.name=data.name,p.value=multcomp, 
        p.adjust.method=p.method)
  class(result) <- "pairwise.htest"
  return(result)
}
