print.cramer.coeff <-
function (x,...) {
  cat("\n        Cramer's association coefficient\n\n")
  cat(x$data.name,"\n")
  cat(paste("X-squared = ",format(x$statistic,digits=5),", df = ",x$parameter,
    ", p-value = ",format(x$p.value,digits=4),"\n",sep=""))
  cat("alternative hypothesis: true association is not equal to 0\n")
  cat(paste(100*x$conf.level," percent confidence interval (",x$rep," replicates):\n",sep=""))
  cat(" ",x$interval[1],"  ",x$interval[2],"\n",sep="")
  cat("sample estimates:\n")
  print(x$estimate,digits=7)
  cat("\n")
}
