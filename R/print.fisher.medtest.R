print.fisher.medtest <-
function(x,...) {
  cat("\nComparison of 2 medians by Fisher's exact test for count data\n\n")
  cat(paste("datas:",x$datas[1],"and",x$datas[2],"\n"))
  cat(paste("odds ratio:",round(x$odds.ratio,5),"\n"))
  cat(paste("p-value:",round(x$p.value,5),"\n\n"))
}

