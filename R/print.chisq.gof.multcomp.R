print.chisq.gof.multcomp <-
function(x,...) {
  cat(paste("\nPairwise comparisons after chi2 goodness-of-fit test\n  (correction: ",x$p.method,")\n\n",sep=""))
  print(x$comp,digits=5)
  cat("\n")
}

