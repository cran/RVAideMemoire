print.chisq.multcomp <-
function(x,...) {
  cat(paste("\nPairwise comparisons after a chi-squared goodness-of-fit test\n  (correction: ",x$p.adjust.method,")\n\n",sep=""))
  print(x$p.value,digits=5,na.print="-")
  cat("\n")
}
