scatterutil.eti.circ2 <- function (x,y,label,clabel,origin=c(0,0),boxes=TRUE,col){
  if (is.null(label)) return(invisible())
  if (any(is.na(label))) return(invisible())
  if (any(label == "")) return(invisible())
  xref <- x-origin[1]
  yref <- y-origin[2]
  for (i in 1:(length(x))) {
    cha <- as.character(label[i])
    cha <- paste(" ",cha," ",sep = "")
    cex0 <- par("cex")*clabel[i]
    xh <- strwidth(cha,cex=cex0)
    yh <- strheight(cha,cex=cex0)*5/6
    if ((xref[i]>yref[i]) & (xref[i]>-yref[i])) {
	x1 <- x[i]+xh/2
	y1 <- y[i]
    } else if ((xref[i]>yref[i]) & (xref[i]<=(-yref[i]))) {
	x1 <- x[i]
	y1 <- y[i]-yh
    } else if ((xref[i]<=yref[i]) & (xref[i]<=(-yref[i]))) {
	x1 <- x[i]-xh/2
	y1 <- y[i]
    } else if ((xref[i]<=yref[i]) & (xref[i]>(-yref[i]))) {
	x1 <- x[i]
	y1 <- y[i]+yh
    }
    if (boxes) rect(x1-xh/2,y1-yh,x1+xh/2,y1+yh,col="white",border=col[i])
    text(x1,y1,cha,cex=cex0,col=col[i])
  }
}

s.corcircle2 <- function (dfxy,xax=1,yax=2,label=row.names(df),clabel=1,
  grid=TRUE,sub="",csub=1,possub="bottomleft",cgrid=0,fullcircle=TRUE,box=FALSE,
  add.plot=FALSE,circle=TRUE,boxes=FALSE,arrows=TRUE,col="black",lwd=1,cpoint=0.8) {
  if (length(clabel)>1 & length(clabel)!=nrow(dfxy)) {stop("incorrect number of character sizes")}
  if (length(col)>1 & length(col)!=nrow(dfxy)) {stop("incorrect number of colors")}
  if (length(lwd)>1 & length(lwd)!=nrow(dfxy)) {stop("incorrect number of line widths")}
  if (length(cpoint)>1 & length(cpoint)!=nrow(dfxy)) {stop("incorrect number of point sizes")}
  if (length(clabel)==1) {clabel <- rep(clabel,nrow(dfxy))}
  if (length(col)==1) {col <- rep(col,nrow(dfxy))}
  if (length(lwd)==1) {lwd <- rep(lwd,nrow(dfxy))}
  if (length(cpoint)==1) {cpoint <- rep(cpoint,nrow(dfxy))}
  arrow1 <- function(x0,y0,x1,y1,len=0.1,ang=15,edge,col,lwd) {
    d0 <- sqrt((x0-x1)^2 + (y0-y1)^2)
    if (d0 < 1e-07) return(invisible())
    segments(x0,y0,x1,y1,lwd=lwd,col=col)
    h <- strheight("A",cex=par("cex"))
    if (d0 > 2 * h) {
	x0 <- x1-h * (x1-x0)/d0
	y0 <- y1-h * (y1-y0)/d0
	if (edge) arrows(x0,y0,x1,y1,angle=ang,length=len,lwd=lwd,col=col)
    }
  }
  point1 <- function(x,y,col,cex) {
    points(x,y,pch=16,col=col,cex=cex)
  }
  scatterutil.circ <- function(cgrid,h,grid,lwd=1,circle) {
    cc <- seq(from=-1,to=1,by=h)
    col <- "lightgray"
    if (grid) {
	for (i in 1:(length(cc))) {
	  x <- cc[i]
	  a1 <- sqrt(1-x*x)
	  a2 <- (-a1)
	  segments(x,a1,x,a2,col=col,lwd=lwd)
	  segments(a1,x,a2,x,col=col,lwd=lwd)
	}
    }
    if (circle) {symbols(0,0,circles=1,inches=FALSE,add=TRUE)}
    segments(-1,0,1,0)
    segments(0,-1,0,1)
    if (cgrid<=0 | !grid) return(invisible())
    cha <- paste("d = ",h,sep="")
    cex0 <- par("cex") * cgrid
    xh <- strwidth(cha,cex=cex0)
    yh <- strheight(cha,cex=cex0) + strheight(" ",cex=cex0)/2
    x0 <- strwidth(" ",cex=cex0)
    y0 <- strheight(" ",cex=cex0)/2
    x1 <- par("usr")[2]
    y1 <- par("usr")[4]
    rect(x1-x0, y1-y0,x1-xh-x0,y1-yh-y0,col="white",border=0)
#   text(x1-xh/2-x0/2,y1-yh/2-y0/2,cha,cex=cex0)
  }
  origin <- c(0,0)
  df <- data.frame(dfxy)
  if (!is.data.frame(df)) stop("Non convenient selection for df")
  if ((xax<1) || (xax>ncol(df))) stop("Non convenient selection for xax")
  if ((yax<1) || (yax>ncol(df))) stop("Non convenient selection for yax")
  x <- df[,xax]
  y <- df[,yax]
  if (add.plot) {
    for (i in 1:length(x)) {
	if (arrows) {
	  arrow1(0,0,x[i],y[i],len=0.1,ang=15,edge=TRUE,col=col[i],lwd=lwd[i])
	} else {
	  point1(x[i],y[i],col=col[i],cex=cpoint[i])
	}
    }
    if (any(clabel>0)) scatterutil.eti.circ2(x,y,label=label,clabel=clabel,col=col,boxes=boxes)
    return(invisible())
  }
  opar <- par(mar=par("mar"))
  on.exit(par(opar))
  par(mar=c(0.1,0.1,0.1,0.1))
  x1 <- x
  y1 <- y
  x1 <- c(x1,-0.01,+0.01)
  y1 <- c(y1,-0.01,+0.01)
  if (fullcircle) {
    x1 <- c(x1,-1,1)
    y1 <- c(y1,-1,1)
  }
  x1 <- c(x1-diff(range(x1)/20),x1+diff(range(x1))/20)
  y1 <- c(y1-diff(range(y1)/20),y1+diff(range(y1))/20)
  plot(x1,y1,type="n",ylab="",asp=1,xaxt="n",yaxt="n",frame.plot=FALSE)
  scatterutil.circ(cgrid=cgrid,h=0.2,grid=grid,circle=circle)
  for (i in 1:length(x)) {
    if (arrows) {
	arrow1(0,0,x[i],y[i],len=0.1,ang=15,edge=TRUE,col=col[i],lwd=lwd[i])
    } else {
	point1(x[i],y[i],col=col[i],cex=cpoint[i])
    }
  }
  if (any(clabel>0)) scatterutil.eti.circ2(x,y,label=label,clabel=clabel,origin,col=col,boxes=boxes)
  if (csub>0) ade4::scatterutil.sub(sub,csub,possub)
  if (box) box()
  invisible(match.call())
}

