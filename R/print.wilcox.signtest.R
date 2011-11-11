print.wilcox.signtest <-
function(x,...) {
  if (is.null(x$mu)) {
    cat("\nComparison of 2 medians by Wilcoxon sign test\n\n")
    cat(paste("data:",x$data[1],"and",x$data[2],"\n"))
    cat(paste("p-value:",round(x$p.value,5),"\n\n"))
  } else {
    cat("\nComparison of one median to a given value\n  by Wilcoxon sign test\n\n")
    cat(paste("data:",x$data,"\n"))
    cat(paste("mu:",x$mu,"\n"))
    cat(paste("p-value:",round(x$p.value,5),"\n\n"))
  }
}

