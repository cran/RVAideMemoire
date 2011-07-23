print.fisher.multcomp <-
function(x,...) {
  cat(paste("\nPairwise comparisons by Fisher's exact test for count data\n  (correction: ",x$p.method,")\n\n",sep=""))
  if (is.data.frame(x$limited.recap)) {
    print(x$limited.recap,digits=5)
  } else {
    cat(paste(x$limited.recap[1],"\n"))
  }
  cat("\n")
}

