pairwise.perm.var.test <- function(resp,fact,p.method="fdr",alternative=c("two.sided","less","greater"),
  nperm=999,progress=TRUE) {
  if (length(resp)!=length(fact)) {
    stop(paste("'",deparse(substitute(resp)),"' and '",deparse(substitute(fact)),
	"' lengths differ",sep=""))
  }
  if (!is.numeric(resp)) {resp <- as.numeric(as.character(resp))}
  if (!is.factor(fact)) {fact <- factor(fact)}
  if (length(alternative)>1) {alternative <- "two.sided"}
  data.name <- paste(deparse(substitute(resp))," and ",deparse(substitute(fact)),"\n",nperm," permutations",sep="")
  method <- "permutation F tests to compare two variances"
  fun.p <- function(i,j) {
    resp2 <- resp[as.numeric(fact)%in%c(i,j)]
    fact2 <- droplevels(fact[as.numeric(fact)%in%c(i,j)])
    perm.var.test(resp2~fact2,alternative=alternative,nperm=nperm,progress=progress)$p.value
  }
  multcomp <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
  result <- list(method=method,data.name=data.name,p.value=multcomp,p.adjust.method=p.method,
    permutations=nperm)
  class(result) <- "pairwise.htest"
  return(result)
}
