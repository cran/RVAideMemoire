pairwise.manova <- function(resp,fact,p.method="fdr") {
  if (nrow(resp)!=length(fact)) {
    stop(paste("'",deparse(substitute(resp)),"' and '",deparse(substitute(fact)),
	"' lengths differ",sep=""))
  }
  if (!is.matrix(resp)) {resp <- as.matrix(resp)}
  if (!is.factor(fact)) {fact <- factor(fact)}
  data.name <- paste(deparse(substitute(resp))," and ",deparse(substitute(fact)),sep="")
  fun.p <- function(i,j) {
    resp2 <- resp[as.numeric(fact)%in%c(i,j),]
    fact2 <- droplevels(fact[as.numeric(fact)%in%c(i,j)])
    summary(manova(resp2~fact2))$stats[1,"Pr(>F)"]
  }
  multcomp <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
  result <- list(method="MANOVAs",data.name=data.name,p.value=multcomp,p.adjust.method=p.method)
  class(result) <- "pairwise.htest"
  return(result)
}
