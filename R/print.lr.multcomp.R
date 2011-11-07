print.lr.multcomp <-
function(x,...) {
  cat(paste("\nComparison of",x$n.reg,"least rectangles simple linear regressions\n\n"))
  cat(paste(x$data[1],"~",x$data[2],", factor =",x$data[3],"\n\n"))
  print(x$comp)
  cat("\n")
}

