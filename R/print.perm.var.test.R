print.perm.var.test <- function(x,...) {
  cat("\n Permutational F test to compare two variances \n\n")
  cat(x$data.name,"\n")
  cat(paste(x$permutations," permutations\n",sep=""))
  cat(paste("F = ",round(x$statistic,4),", p-value = ",format(x$p.value,digits=6,nsmall=6),"\n",sep=""))
  cat(x$H1,"\n")
  cat("sample estimates:\n")
  print(x$estimate)
  cat("\n")
}
