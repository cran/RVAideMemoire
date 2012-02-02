print.fisher.multcomp <-
function(x,...) {
  cat(paste("\nPairwise comparisons by Fisher's exact test for count data\n  (correction: ",x$p.adjust.method,")\n\n",sep=""))
  print(x$p.value,digits=4)
  cat("\n")
}

