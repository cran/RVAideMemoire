mod <-
function (x) {
  x <- na.omit(x)
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
    abs(x - round(x)) < tol
  }
  result <- NULL
  if (all(is.wholenumber(x))) {
    result <- rle(sort(x))$values[which.max(rle(sort(x))$lengths)]
  } else {
    dens <- density(x)
    result <- dens$x[which(dens$y == max(dens$y))]
  }
  return(result)
}