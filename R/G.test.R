G.test <- 
function(x,p=rep(1/length(x),length(x))) {
  data.name <- deparse(substitute(x))
  if (is.table(x) | is.matrix(x)) {
    chi <- suppressWarnings(chisq.test(x))
    method <- "G-test"
  } else if (is.vector(x)) {
    chi <- suppressWarnings(chisq.test(x,p=p))
    method <- "G-test for given probabilities"
  }
  theo <- chi$expected
  G <- 2*sum(x*log(x/theo))
  names(G) <- "G"
  ddl <- chi$parameter
  p <- pchisq(G,ddl,lower.tail=FALSE)
  result <- list(method=method,statistic=G,parameter=ddl,p.value=p,
    data.name=data.name,observed=x,expected=theo)
  class(result) <- "htest"
  return(result)
}
