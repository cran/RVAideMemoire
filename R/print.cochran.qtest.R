print.cochran.qtest <-
function(x,...) {
  cat("\n    Cochran's Q test\n\n")
  cat(paste(x$datas[1],"~",x$datas[2],", block =",x$datas[3],"\n\n"))
  print(x$tab.test,digits=5)
  cat("\n")
  if (x$p.value<x$alpha) {
    cat(paste("Pairwise comparisons by sign test - correction:",x$p.method,"\n"))
    print(x$multcomp,digits=5)
    cat("\n")
  }
}

