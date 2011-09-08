print.wilcox.paired.multcomp <-
function (x,...) {
  cat(paste("\nPairwise comparisons by Wilcoxon signed rank test\n  (correction: ",x$p.method,")\n\n",sep=""))
  cat(paste(x$data[1],"~",x$data[2],", block =",x$data[3],"\n\n"))
  print(x$comp,digits=5)
  cat("\n")
}

