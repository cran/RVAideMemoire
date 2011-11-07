print.cor.multcomp <-
function(x,...) {
  cat(paste("\n Comparison of",x$levels,"Pearson's linear correlation coefficients\n\n"))
  print(x$comp,digits=5)
  cat("\n")
  if (x$p.comp>x$alpha){
    cat(paste("Common correlation coefficient, ",100*x$conf.level,"% confidence interval\nand equality to given value ",x$r.theo,"\n",sep=""))
    print(x$comm,digits=5)
    cat("\n")
  }
  if (x$p.comp<x$alpha & x$levels>2) {
    cat(paste("Pairwise comparisons - correction:",x$p.method,"\n"))
    print(x$multcomp,digits=5)
    cat("\n")
  }
}

