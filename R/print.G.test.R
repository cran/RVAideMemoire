print.G.test <- function(x,...) {
  cat(paste("\n        ",x$method,"\n\n"))
  cat(paste("data:",x$data.name,"\n"))
  cat(paste("G = ",format(x$statistic,digits=5),", df = ",x$parameter,
    ", p-value = ",format(x$p.value,digits=5),"\n\n",sep=""))
}
