print.chisq.bintest <-
function(x,...) {
  cat("\n",x$method,"\n\n")
  cat("data:",x$data.name,"\n")
  if (x$test=="Chi-squared") {
    cat(paste("X-squared = ",format(x$statistic,digits=3),", df = ",x$parameter,", p-value = ",
	format(x$p.value,digits=4),sep=""))
    cat("\n")
  } else {
    cat(paste("p-value = ",format(x$p.value,digits=4),sep=""))
    cat("\n")
  }
  if (x$p.value<x$alpha) {
    cat("\n",x$multcomp.method,"\n")
    print(x$multcomp,na.print="-",digits=5)
  }
  cat("\n")
}
