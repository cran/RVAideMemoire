wilcox.signtest <-
function(x,y,mu=NULL) {
  if (is.null(mu)) {
    if (length(x)!=length(y)) {stop("'x' and 'y' lengths differ")}
    data.name <- paste("data: ",deparse(substitute(x))," and ",deparse(substitute(y)),sep="")
    method <- "Comparison of 2 medians by Wilcoxon sign test"
    if (any(is.na(x))) {
	y <- y[-which(is.na(x))]
	x <- x[-which(is.na(x))]
    }
    if (any(is.na(y))) {
	x <- x[-which(is.na(y))]
	y <- y[-which(is.na(y))]
    }
    signs <- x-y
    p <- binom.test(length(signs[signs>0]),length(signs[signs!=0]),0.5)$p.value
  } else {
    data.name <- paste("data: ",deparse(substitute(x)),sep="")
    method <- "Comparison of one median to a given value\n  by Wilcoxon sign test"
    if (any(is.na(x))) {x <- x[-which(is.na(x))]}
    signs <- x-mu
    p <- binom.test(length(signs[signs>0]),length(signs[signs!=0]),0.5)$p.value
  }
  result <- list(method=method,data.name=data.name,mu=mu,p.value=p)
  class(result) <- c("wilcox.signtest","list")
  return(result)
}
