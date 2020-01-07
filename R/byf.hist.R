# grDevices: col2rgb, n2mfrow, palette, rgb

byf.hist <- function(formula,data,sep=FALSE,density=TRUE,xlab=NULL,ylab=NULL,col=NULL) {
  if (missing(formula)||(length(formula)!=3)) {stop("missing or incorrect formula")}
  m <- match.call(expand.dots=FALSE)
  m$sep <- m$density <- m$col <- NULL
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  m$... <- m$xlab <- m$ylab <- NULL
  mf <- eval(m,parent.frame())
  dname <- c(names(mf)[1],paste(names(mf)[2:ncol(mf)],collapse=":"))
  resp <- mf[,1]
  fact <- interaction(mf[,2:ncol(mf)],sep=":")
  nlev <- nlevels(fact)
  if (is.null(xlab)) {xlab <- dname[1]}
  if (is.null(ylab)) {ylab <- "Density"}
  if (sep) {
    opar <- par(no.readonly=TRUE)
    on.exit(par(opar))
    par(mfrow=grDevices::n2mfrow(nlevels(fact)))
    if (density) {
	if (is.null(col)) {col <- rep("black",nlev)}
	if (length(col)==1) {col <- rep(col,nlev)}
	for (i in 1:nlev) {
	  y <- resp[as.numeric(fact)==i]
	  h <- suppressWarnings(hist(y,freq=FALSE,plot=FALSE))
	  plot(0,xlim=range(h$breaks),ylim=c(0,max(h$density)),xlab=xlab,
	    ylab=ylab,main=levels(fact)[i],cex=0)
	  dens <- density(y)
	  col2 <- grDevices::col2rgb(col[i])
	  col3 <- grDevices::rgb(col2[1,],col2[2,],col2[3,],alpha=0.4*255,maxColorValue=255)
	  polygon(dens$x,dens$y,col=col3,border=NA)
	  rug(y,col=i)
	}
    } else {
	if (is.null(col)) {col <- rep("white",nlev)}
	if (length(col)==1) {col <- rep(col,nlev)}
	for (i in 1:nlevels(fact)) {
	  y <- resp[as.numeric(fact)==i]
	  hist(y,xlab=xlab,main=levels(fact)[i],col=col[i])
	}
    }
  } else {
    if (density) {
	if (is.null(col)) {col <- 1:nlev}
	if (length(col)==1) {col <- rep(col,nlev)}
	dhist(resp,fac=fact,col=col,legend=TRUE,pos.legend="topright",xlab=xlab,ylab=ylab)
    } else {
	if (is.null(col)) {col <- rep(grDevices::palette(),nlev%/%length(grDevices::palette())+1)[1:nlev]}
	if (length(col)==1) {col <- rep(col,nlev)}
	angles <- seq(20,160,140/(nlev-1))
	dens <- integer(nlev)
	for (i in 1:nlev) {
	  x.i <- resp[as.numeric(fact)==i]
	  dens[i] <- max(hist(x.i,plot=FALSE)$counts)
	}
	x.1 <- resp[as.numeric(fact)==1]
	hist(x.1,xlab=xlab,ylab=ylab,density=10,angle=angles[1],col=col[1],
	  xlim=range(resp),ylim=c(0,max(dens)),main="")
	box()
	for (i in 2:nlev) {
	  x.i <- resp[as.numeric(fact)==i]
	  hist(x.i,xlab="",ylab="",main="",density=10,angle=angles[i],col=col[i],
	    add=TRUE)
	}
	legend("topright",levels(fact),fill=col)
    }
  }
}

