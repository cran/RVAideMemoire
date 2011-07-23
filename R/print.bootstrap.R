print.bootstrap <-
function(x,...) {
  cat(paste(100*x$conf.level,"% confidence interval (",x$rep," replicates)\n",sep=""))
  print(x$interval,digits=5)
}

