G.theo.multcomp <-
function(x,p=rep(1/length(x),length(x)),p.method="fdr") {
  if (sum(p)!=1) {stop("sum of probabilities must be 1")}
  theo <- integer(length(x))
  G <- integer(length(x))
  pval <- integer(length(x))
  for (i in 1:length(x)) {
    test <- G.test(c(x[i],sum(x)-x[i]),p=c(p[i],1-p[i]))
    theo[i] <- as.numeric(test$expected[1])
    G[i] <- as.numeric(test$statistic)
    pval[i] <- as.numeric(test$p.value)
  }
  p.adj <- p.adjust(pval,method=p.method)
  comp <- data.frame("observed"=x,"expected"=theo,"G"=G,"Pr(>G)"=p.adj," "=psignif(p.adj),
    stringsAsFactors=FALSE,check.names=FALSE)
  result <- list(observed=x,expected=theo,p.adjust.method=p.method,statistic=G,p.value=p.adj,comp=comp)
  class(result) <- c("G.theo.multcomp","list")
  return(result)
}

