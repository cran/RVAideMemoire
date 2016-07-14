# dunn.test : dunn.test

dunn.test <- function(resp,fact,p.method="fdr",alpha=0.05) {
  if (length(resp)!=length(fact)) {
    stop(paste("'",deparse(substitute(resp)),"' and '",deparse(substitute(fact)),
	"' lengths differ",sep=""))
  }
  if (!is.numeric(resp)) {resp <- as.numeric(as.character(resp))}
  if (!is.factor(fact)) {fact <- factor(fact)}
  data.name <- paste(deparse(substitute(resp)),"and",deparse(substitute(fact)))
  method <- "Dunn's test"
  p.method2 <- p.method
  if (p.method2 %in% c("fdr","BH")) {p.method2 <- "bh"}
  if (p.method2=="BY") {p.method2 <- "by"}
  trash <- capture.output(test <- dunn.test::dunn.test(resp,fact,method=p.method2,alpha=alpha,kw=FALSE))
  nlev <- nlevels(fact)
  multcomp <- matrix(NA,nrow=nlev-1,ncol=nlev-1,dimnames=list(levels(fact)[-1],levels(fact)[-nlev]))
  for (i in 1:length(test$comparisons)) {
    comp <- test$comparisons[i]
    lev <- strsplit(comp," - ")[[1]]
    multcomp[lev[2],lev[1]] <- test$P.adjusted[i]
  }
  result <- list(method=method,data.name=data.name,p.value=multcomp,p.adjust.method=p.method)
  class(result) <- "pairwise.htest"
  return(result)
}
