byf.mqqnorm <- function(resp,fact) {
  if (nrow(resp)!=length(fact)) {
    stop(paste("'",deparse(substitute(resp)),"' and '",deparse(substitute(fact)),"' lengths differ",sep=""))
  }
  if (!is.matrix(resp)) {resp <- as.matrix(resp)}
  resp <- t(resp)
  if (!is.factor(fact)) {fact <- factor(fact)}
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  par(mfrow=n2mfrow(nlevels(fact)))
  for (i in 1:nlevels(fact)) {
    mqqnorm(resp[,as.numeric(fact)==i],main=levels(fact)[i])
  }
}
