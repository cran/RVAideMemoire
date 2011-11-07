print.reg.intcomp <-
function(x,...) {
  cat(paste("\n",x$data[1],"~",x$data[2],", factor =",x$data[3],"\n\n"))
  cat(paste("Common slope and ",100*x$conf.level,"% confidence interval\n",sep=""))
  print(x$slope.comm,digits=5)
  cat(paste("\nIntercepts, ",100*x$conf.level,"% confidence intervals and equality to given values\n",sep=""))
  print(x$intercepts.tab,digits=5)
  cat(paste("\nPairwise comparisons - correction:",x$p.method,"\n"))
  print(x$multcomp,digits=5)
  cat("\n")
}

