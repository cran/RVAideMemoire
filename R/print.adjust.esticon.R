print.adjust.esticon <-
function(x,...) {
  cat(paste("\nPairwise comparisons - correction:",x$p.adjust.method,"\n\n"))
  print(x$tab.adjust,digits=5)
  cat("\n")
}

