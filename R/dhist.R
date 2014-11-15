dhist <- function(x,fac,col,legend,pos.legend,xlab,...) {
  ymax <- integer(nlevels(fac))
  for (i in 1:nlevels(fac)) {
    ymax[i] <- suppressWarnings(max(hist(x[as.numeric(fac)==i],
	freq=FALSE,plot=FALSE)$density))
  }
  h <- suppressWarnings(hist(x,freq=FALSE,plot=FALSE))
  plot(0,xlim=range(h$breaks),ylim=c(0,max(ymax)),xlab=xlab,
    ylab="Density",cex=0,...)
  dens <- tapply(x,fac,function(x) density(x))
  if (!is.numeric(col)) {
    col3 <- col4 <- col
  } else {
    col2 <- col2rgb(palette()[col])
    col3 <- apply(col2,2,function(x) rgb(x[1],x[2],x[3],alpha=0.4*255,maxColorValue=255))
    col4 <- apply(col2,2,function(x) rgb(x[1],x[2],x[3],alpha=255,maxColorValue=255))  
  }
  for (i in 1:nlevels(fac)) {
    d <- dens[[i]]
    polygon(d$x,d$y,col=col3[i],border=NA)
    rug(x[as.numeric(fac)==i],col=col4[i])
  }
  if (legend) {
    legend(pos.legend,levels(fac),fill=col3)
  }
}
