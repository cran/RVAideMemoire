print.wilcox.signtest <-
function(x,...) {
  cat("\n",x$method,"\n\n")
  cat(x$data.name,"\n")
  if (!is.null(x$mu)) {cat(paste("mu:",x$mu,"\n"))}
  cat(paste("p-value:",round(x$p.value,5),"\n\n"))
}

