print.cor.2comp <-
function(x,...) {
  cat("\n Comparison of 2 Pearson's linear correlation coefficients\n\n")
  print(x$comp,digits=5)
  cat("\n")
  if (x$p.comp>x$alpha){
    cat(paste("Common correlation coefficient, ",100*x$conf.level,"% confidence interval\nand equality to theoretical value ",x$r.theo,"\n",sep=""))
    print(x$comm,digits=5)
    cat("\n")
  }
}

