byf.qqnorm <- function(x,f,...) {
  if (!is.factor(f)) {f <- factor(f)}
  par(mfrow=n2mfrow(nlevels(f)))
  for (i in 1:nlevels(f)) {
    qqnorm(x[as.numeric(f)==i],main=levels(f)[i],...)
  }
}
