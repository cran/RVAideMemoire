print.perm.anova <- function(x,...) {
  cat("Permutational Analysis of Variance Table\n")
  cat(paste(x$permutations," permutations\n\n",sep=""))
  cat(x$data.name,"\n")
  print(x$table,na.print=" ")
  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}
