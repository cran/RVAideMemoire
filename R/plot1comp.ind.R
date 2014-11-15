plot1comp.ind <- function(dfxy,fac,axis=1,col=1:nlevels(fac),legend=TRUE,
  pos.legend="topright",...) {
  dhist(dfxy[,axis],fac=fac,col=col,legend=legend,pos.legend=pos.legend,
    xlab=paste("Axis",axis),...)
}
