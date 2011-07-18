print.spearman.ci <-
function(x,...) {
  cat(paste("Spearman's rank correlation\nrho =",round(x$coeff,5),"\n\n"))
  cat(paste(100*x$conf.level,"% confidence interval (",x$rep," replicates)\n",sep=""))
  print(x$interval,digits=5)
}

