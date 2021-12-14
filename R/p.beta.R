p.beta <- function(p,n=length(p),C=2,back=FALSE) {
  if (min(p,na.rm=TRUE)<0 | max(p,na.rm=TRUE)>1) {
    stop(paste(deparse(substitute(p)),"is not bounded between 0 and 1"))
  }
  res <- if (!back) {
    (p*(n-1)+1/C)/n
  } else {
    (p*n-(1/C))/(n-1)
  }
  return(res)
}
