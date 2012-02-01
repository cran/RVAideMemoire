print.perm.wilcox.test <- function(x,...) {
  cat("\n",x$method,"\n\n")
  cat(x$data.name,"\n")
  cat(paste(x$permutations," permutations\n",sep=""))
  cat(paste(names(x$statistic)," = ",round(x$statistic,4),", p-value = ",format(x$p.value,digits=6,nsmall=6),"\n",sep=""))
  cat(x$H1,"\n")
  cat("\n")
}
