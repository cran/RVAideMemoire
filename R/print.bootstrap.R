print.bootstrap <-
function(x,...) {
  cat(paste("\n",100*x$conf.level,"% confidence interval (",x$rep," replicates)\n\n",sep=""))
  print(x$interval,digits=5)
  cat("\n")
}

