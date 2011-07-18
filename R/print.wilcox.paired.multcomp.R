print.wilcox.paired.multcomp <-
function (x,...) {
  cat(paste("\nPairwise comparisons by Wilcoxon signed rank test\n  (correction: ",x$p.method,")\n\n",sep=""))
  cat(paste(x$datas[1],"~",x$datas[2],", block =",x$datas[3],"\n\n"))
  print(x$comp,digits=5)
  cat("\n")
}

