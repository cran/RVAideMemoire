print.prop.multcomp <-
function(x,...) {
  cat(paste("\nPairwise comparisons to expected proportions\n  by exact binomial test  (correction: ",x$p.adjust.method,")\n\n",sep=""))
  print(x$comp,digits=5)
  cat("\n")
}

