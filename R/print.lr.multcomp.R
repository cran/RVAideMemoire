print.lr.multcomp <-
function(x,...) {
  cat(paste("\nComparison of",x$n.reg,"least rectangles simple linear regressions\n\n"))
  print(x$comp)
  cat("\n")
}

