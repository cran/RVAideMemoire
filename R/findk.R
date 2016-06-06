findk <- function(set,f,k) {
  f <- droplevels(factor(f))
  n <- table(droplevels(factor(f)))
  if (k>min(n)) {k <- min(n)}
  spl <- splitf(set,f,k)
  if (k!=length(spl)) {k <- length(spl)}
  return(k)
}
