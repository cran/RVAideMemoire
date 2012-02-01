print.cochran.qtest <-
function(x,...) {
  cat("\n    Cochran's Q test\n\n")
  cat(x$data.name,"\n\n")
  print(x$tab.test,digits=5,row.names=FALSE)
  cat("\n")
  if (x$p.value<x$alpha) {
    cat(paste("Pairwise comparisons by Wilcoxon sign test - correction:",x$p.adjust.method,"\n"))
    print(x$multcomp,digits=5,na.print="-")
    cat("\n")
  }
}

