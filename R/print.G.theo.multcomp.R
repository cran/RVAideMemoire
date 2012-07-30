print.G.theo.multcomp <-
function(x,...) {
  cat(paste("\nPairwise comparisons after a G-test for given probabilities\n  (correction: ",x$p.adjust.method,")\n\n",sep=""))
  print(x$comp,digits=5,row.names=FALSE)
  cat("\n")
}

