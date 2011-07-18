print.cor.conf <-
function(x,...) {
  cat(paste("Equality of Pearson's linear correlation coefficient to",x$r.theo,"\n\n"))
  print(x$conform,digits=5)
}

