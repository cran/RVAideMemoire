print.ind.contrib <-
function(x,...) {
  if (x$print.diff==TRUE) {
    print(x$coefficients.prop,digits=5)
  }
}

