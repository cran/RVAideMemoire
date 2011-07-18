print.cramer.cor <-
function(x,...) {
  cat(paste("Cramer's association coefficient\nV =",round(x$V,5),"\n\n"))
  cat(paste(100*x$conf.level,"% confidence interval (",x$rep," replicates)\n",sep=""))
  print(x$interval,digits=5)
}

