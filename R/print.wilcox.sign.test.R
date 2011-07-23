print.wilcox.sign.test <-
function(x,...) {
  if (!is.null(x$mu)) {
    cat("\nComparison of one median to a theoretical value\n  by Wilcoxon sign test\n\n")
    cat(paste("data:",x$datas,"\n"))
    cat(paste("mu:",x$mu,"\n"))
    cat(paste("p-value:",round(x$p.value,5),"\n\n"))
  } else {
    cat("\nComparison of 2 medians by Wilcoxon sign test\n\n")
    cat(paste("datas:",x$datas[1],"and",x$datas[2],"\n"))
    cat(paste("p-value:",round(x$p.value,5),"\n\n"))
  }
}

