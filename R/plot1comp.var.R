plot1comp.var <- function(dfxy,axis=1,cex=1) {
  opar <- par()
  on.exit(suppressWarnings(par(opar)))
  n <- nrow(dfxy)
  par(mar=c(5,6,4,2))
  plot(dfxy[,axis],1:n,xlim=c(-1,1),xlab=paste("Correlation with axis",axis),ylab="",cex=0,axes=FALSE)
  box()
  axis(1)
  mtext(rownames(dfxy),side=2,line=1,at=1:n,las=2,cex=cex)
  abline(v=0,col="grey")
  abline(h=1:n,col="grey",lty=3)
  arrows(0,1:n,dfxy[1:n,axis],1:n,length=0.1)
}
