print.reg.intcomp <-
function(x,...) {
  cat("\n",x$data.name,"\n\n")
  cat(paste("Common slope and ",100*x$conf.level,"% confidence interval\n",sep=""))
  print(x$slope.comm,digits=5)
  cat(paste("\nIntercepts, ",100*x$conf.level,"% confidence intervals and equality to given values\n",sep=""))
  print(x$intercepts.tab,digits=5)
  cat(paste("\nPairwise comparisons - correction:",x$p.adjust.method,"\n"))
  print(x$multcomp,digits=5,na.print="-")
  cat("\n")
}

