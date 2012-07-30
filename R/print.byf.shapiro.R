print.byf.shapiro <- function(x,...) {
  cat(paste("data: ",x$data.name,"\n\n",sep=""))
  print(x$tab.result)
}
