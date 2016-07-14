cdf.discrete <- function(x,dist=c("binom","geom","hyper","nbinom","pois"),...) {
  dist <- match.arg(dist)
  x <- sort(x)
  n <- length(x)
  if (n<1) stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  fun <- switch(dist,binom=pbinom,geom=pgeom,hyper=phyper,nbinom=pnbinom,pois=ppois)
  rval <- approxfun(vals,fun(vals,...),method="constant",yleft=0,yright=1,f=0,
    ties="ordered")
  class(rval) <- c("ecdf","stepfun",class(rval))
  assign("nobs",length(x),envir=environment(rval))
  attr(rval,"call") <- sys.call()
  return(rval)
}
