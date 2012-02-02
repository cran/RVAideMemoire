print.reg.slpcomp <-
function(x,...) {
  cat("\n",x$data.name,"\n\n")
  cat(paste("Slopes and ",100*x$conf.level,"% confidence intervals\n",sep=""))
  print(x$coeffs.tab,digits=5)
  cat(paste("\nPairwise comparisons - correction: ",x$p.adjust.method,"\n"))
  print(x$multcomp,digits=5,na.print="-")
  cat("\n")
}

