print.spearman.ci <-
function(x,...) {
  cat("\nSpearman's rank correlation\n\n")
  cat(paste("rho = ",round(x$coeff,5),"\n\n",sep=""))
  cat(paste(100*x$conf.level,"% confidence interval (",x$rep," replicates)\n",sep=""))
  print(x$interval,digits=5)
  cat("\n")
}

