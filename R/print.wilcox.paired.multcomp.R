print.wilcox.paired.multcomp <-
function (x,...) {
  cat(paste("\nPairwise comparisons by ",x$method,"\n  (correction: ",x$p.adjust.method,")\n\n",sep=""))
  cat(x$data.name,"\n\n")
  print(x$comp,digits=5,na.print="-")
  cat("\n")
}

