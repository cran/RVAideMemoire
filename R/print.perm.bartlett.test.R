print.perm.bartlett.test <- function(x,...) {
  cat("\n Permutational Bartlett's test of homogeneity of variances \n\n")
  cat(x$data.name,"\n")
  cat(paste(x$permutations," permutations\n",sep=""))
  cat(paste("Bartlett's K-squared = ",round(x$statistic,4),", p-value = ",format(x$p.value,digits=6,nsmall=6),"\n",sep=""))
  cat("\n")
}
