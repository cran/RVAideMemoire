print.reg.slpcomp <-
function(x,...) {
  cat(paste("\n",x$data[1],"~",x$data[2],", factor =",x$data[3],"\n\n"))
  cat(paste("Slopes and ",100*x$conf.level,"% confidence intervals\n\n",sep=""))
  print(x$coeffs.tab,digits=5)
  cat(paste("\nPairwise comparisons - correction: ",x$p.method,"\n"))
  print(x$multcomp,digits=5)
  cat("\n")
}

