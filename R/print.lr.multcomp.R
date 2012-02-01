print.lr.multcomp <-
function(x,...) {
  cat(paste("\nComparison of",x$n.reg,"least rectangles simple linear regressions\n\n"))
  cat(x$data.name,"\n\n")
  cat("Intercepts:\n")
  print(noquote(x$intercepts.comp),na.print="-")
  cat("\nSlopes:\n")
  print(noquote(x$slopes.comp),na.print="-")
  cat("\n")
}
