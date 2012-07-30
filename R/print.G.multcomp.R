print.G.multcomp <- function (x,...) {
  cat(paste("\nPairwise comparisons after a G-test\n  (correction: ", 
    x$p.adjust.method, ")\n\n", sep = ""))
  print(x$p.value, digits = 5, na.print = "-")
  cat("\n")
}
