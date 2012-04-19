print.wilcox.signtest <-
function (x,...) {
  cat("\n",x$method,"\n\n")
  cat(x$data.name,"\n")
  if (x$method=="Comparison of one median to a given value\n  by Wilcoxon sign test") {
    cat(paste("mu:",x$mu,"\n"))
  }
  cat(paste("p-value:",format(x$p.value,digits=5),"\n\n"))
}
