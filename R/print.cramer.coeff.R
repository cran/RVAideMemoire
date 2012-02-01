print.cramer.coeff <-
function(x,...) {
  cat(paste("\nCramer's association coefficient\nV =",round(x$statistic,5),"\n\n"))
  cat(paste(100*x$conf.level,"% confidence interval (",x$rep," replicates)\n",sep=""))
  print(x$interval,digits=5)
  cat("\n")
}

