summary.least.rect <- function(object,...) {
  cat("\nCall:\n")
  print(object$call)
  cat("\nResiduals:\n")
  print(summary(object$residuals),digits=3)
  cat("\nCoefficients and ",100*object$conf.level,"% confidence interval:\n",sep="")
  if (!object$multiple) {
    print(object$conf.int,digits=5)
  } else {
    for (i in dimnames(object$conf.int)[[3]]) {
	cat(" ",i,"\n")
	print(object$conf.int[,,i])
    }
  }
  cat("\nEquality of the slope",ifelse(object$multiple,"s","")," to ",object$theo,":\n",sep="")
  printCoefmat(object$comp,na.print="",P.values=TRUE,has.Pvalue=TRUE)
  cat("\nPearson's linear correlation coefficient",ifelse(object$multiple,"s",""),
    " (",100*object$conf.level,"% confidence interval):\n",sep="")
  printCoefmat(object$corr,na.print="",P.values=TRUE,has.Pvalue=TRUE)
  cat("\n")
}
