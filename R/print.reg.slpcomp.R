print.reg.slpcomp <-
function(x,...) {
  cat(paste("\nSlopes and ",100*x$conf.level,"% confidence intervals\n",sep=""))
  print(x$coeffs.tab,digits=5)
  cat(paste("\nPairwise comparisons - correction: ",x$p.method,"\n"))
  print(x$multcomp,digits=5)
  cat("\n")
}

