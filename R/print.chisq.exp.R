print.chisq.exp <-
function(x,...) {
  cat("\nExpected counts\n")
  print(x$mat)
  cat(paste("\nCochran's rule: maximum",x$cochran,"count(s) can be < 5\n\n"))
}

