print.perm.cor.test <- function(x,...) {
  cat("\n Pearson's product-moment correlation - Permutation test\n\n")
  cat(x$data.name,"\n")
  cat(paste(x$permutations," permutations\n",sep=""))
  cat(paste("t = ",round(x$statistic,4),", p-value = ",format(x$p.value,digits=6,nsmall=6),"\n",sep=""))
  cat(x$H1,"\n")
  cat("95 percent confidence interval:\n")
  cat(x$conf.int,"\n")
  cat("sample estimates:\n")
  print(x$estimate)
  cat("\n")
}
