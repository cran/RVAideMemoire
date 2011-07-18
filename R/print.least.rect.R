print.least.rect <-
function(x,...) {
  cat("\n        Least rectangles simple linear regression\n\n")
  cat(paste("Equation :",x$y,"=",round(x$conf.int[1,2],5),"+",round(x$conf.int[2,2],5),x$x,"\n\n"))
  cat(100*x$conf.level,"% confidence interval\n",sep="")
  print(x$conf.int,digits=5)
  cat("\nEquality of the slope to",x$slope.theo,"\n")
  print(x$comp,digits=5)
  cat("\nPearson's linear correlation coefficient\n(",100*x$conf.level,"% confidence interval)\n",sep="")
  print(x$corr,digits=5)
  cat("\n")
}

