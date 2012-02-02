print.perm.kruskal.test <- function(x,...) {
  cat("\n Permutational Kruskal-Wallis rank sum test \n\n")
  cat(x$data.name,"\n")
  cat(paste(x$permutations," permutations\n",sep=""))
  cat(paste("Kruskal-Wallis chi-squared = ",round(x$statistic,4),", p-value = ",format(x$p.value,digits=6,nsmall=6),"\n",sep=""))
  cat("\n")
}
