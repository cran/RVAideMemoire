plot1comp.ind <- function(dfxy,fac,axis=1,col=1:nlevels(fac),cex=1,box=TRUE) {
  plot(dfxy[,axis],rep(1,nrow(dfxy)),cex=0,xlab=paste("Axis",axis),ylab="",
    ylim=c(0.5,1+nlevels(fac)),axes=FALSE)
  box()
  axis(1)
  abline(v=0,col="grey")
  abline(h=1:nlevels(fac),col="grey",lty=3)
  moy <- tapply(dfxy[,axis],fac,mean)
  d <- diff(range(dfxy[,axis]))
  for (i in 1:nlevels(fac)) {
    coord <- dfxy[as.numeric(fac)==i,axis]
    points(coord,rep(i,length(coord)),col=col[i])
    if (box) {
    xl <- moy[i]-d/100*nchar(names(moy)[i])*cex
    xr <- moy[i]+d/100*nchar(names(moy)[i])*cex
    yl <- i+0.5-0.1*cex
    yr <- i+0.5+0.1*cex
    rect(xl,yl,xr,yr,col="white",border=col[i])
    }
    segments(moy[i],i,moy[i],i+0.5-0.1*cex,col=col[i])
    segments(moy[i],0,moy[i],i,col=col[i],lty=2)
    text(moy[i],i+0.5,names(moy)[i],col=col[i],cex=cex)
  }
}
