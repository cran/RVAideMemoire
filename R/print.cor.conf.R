print.cor.conf <-
function(x,...) {
  cat(paste("\nEquality of a Pearson's linear correlation coefficient to",x$r.theo,"\n\n"))
  print(x$conform,digits=5,row.names=FALSE)
  cat("\n")
}

