print.chisq.exp <-
function(x,...) {
  cat("Expected counts\n")
  print(x$mat)
  cat(paste("\nCochran's rule :",x$cochran,"count(s) maximum can be < or = to 5\n"))
}

