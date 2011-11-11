print.chisq.theo.multcomp <-
function(x,...) {
  cat(paste("\nPairwise comparisons after chi-squared test for given probabilities\n  (correction: ",x$p.method,")\n\n",sep=""))
  print(x$comp,digits=5)
  cat("\n")
}

