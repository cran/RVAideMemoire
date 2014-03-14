mod <-
function (x) {
  x <- na.omit(x)
  if (is.numeric(x)) {
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
	abs(x - round(x)) < tol
    }
    if (all(is.wholenumber(x))) {
	reps <- rle(sort(x))
	result <- reps$values[which.max(reps$lengths)]
    } else {
	dens <- density(x)
	result <- dens$x[which(dens$y == max(dens$y))]
    }
  } else if (is.character(x) | is.factor(x)) {
    tab <- table(x)
    wm <- which.max(tab)
    result <- if (length(wm)==1) {
	names(tab)[wm]
    } else {
	paste(names(tab)[wm],collapse=",")
    }
  }
  return(result)
}
