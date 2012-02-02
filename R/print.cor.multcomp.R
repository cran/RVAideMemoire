print.cor.multcomp <-
function(x,...) {
  cat(paste("\n Comparison of",x$levels,"Pearson's linear correlation coefficients\n\n"))
  print(x$comp,digits=5,row.names=FALSE)
  cat("\n")
  if (x$p.value.comp>x$alpha){
    cat(paste("\nCommon correlation coefficient, ",100*x$conf.level,"% confidence interval\nand equality to given value ",x$r.theo,"\n\n",sep=""))
    print(x$comm,digits=5,row.names=FALSE)
    cat("\n")
  }
  if (x$p.value.comp<x$alpha & x$levels>2) {
    cat(paste("\nPairwise comparisons - correction:",x$p.adjust.method,"\n\n"))
    print(x$p.multcomp,digits=5,na.print="-")
    cat("\n")
  }
}

