print.cor.2comp <-
function(x,...) {
  cat("\n Comparison of 2 Pearson's linear correlation coefficients\n\n")
  print(x$comp,digits=5,row.names=FALSE)
  cat("\n")
  if (x$p.comp>x$alpha){
    cat(paste("\nCommon correlation coefficient, ",100*x$conf.level,"% confidence interval\nand equality to given value ",x$r.theo,"\n\n",sep=""))
    print(x$comm,digits=5,row.names=FALSE)
    cat("\n")
  }
}

