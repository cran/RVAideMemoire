byf.hist <- function(x,f,...) {
  if (!is.factor(f)) {f <- factor(f)}
  par(mfrow=n2mfrow(nlevels(f)))
  for (i in 1:nlevels(f)) {
    hist(x[as.numeric(f)==i],xlab=deparse(substitute(x)),main=levels(f)[i],...)
  }
}
