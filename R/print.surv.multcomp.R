print.surv.multcomp <-
function(x,...) {
  cat(paste("\nSurvival analysis - pairwise comparisons (correction: ",x$p.method,")\n\n",sep=""))
  cat(paste(x$model,"\n\n"))
  print(x$comp,digits=5)
  cat("\n")
}

