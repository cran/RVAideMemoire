scatter.coa2 <- function (x,xax=1,yax=2,sep=TRUE,boxes=FALSE,method=1:3,clab.row=0.75,
  clab.col=1.25,posieig="none",sub=NULL,csub=2,...) {
  if (!inherits(x,"dudi")) {stop("Object of class 'dudi' expected")}
  if (!inherits(x,"coa")) {stop("Object of class 'coa' expected")}
  nf <- x$nf
  if ((xax>nf) || (xax<1) || (yax>nf) || (yax<1) || (xax==yax)) {stop("Non convenient selection")}
  method <- method[1]
  if (method==1) {
    coolig <- x$li[,c(xax,yax)]
    coocol <- x$co[,c(xax,yax)]
    names(coocol) <- names(coolig)
  } else if (method==2) {
    coocol <- x$c1[,c(xax,yax)]
    coolig <- x$li[,c(xax,yax)]
    names(coocol) <- names(coolig)
  } else if (method==3) {
    coolig <- x$l1[,c(xax,yax)]
    coocol <- x$co[,c(xax,yax)]
    names(coocol) <- names(coolig)
    } else {stop("Unknown method")}
  if (sep) {
    par(mfrow=c(1,2))
    ade4::s.label(rbind.data.frame(coolig,coocol),clabel=0,cpoint=0,sub=sub,csub=csub)
    ade4::s.label(coocol,boxes=boxes,clabel=clab.row,add.plot=TRUE)
    ade4::s.label(rbind.data.frame(coolig,coocol),clabel=0,cpoint=0,sub=sub,csub=csub)
    ade4::s.label(coolig,boxes=boxes,clabel=clab.row,add.plot=TRUE)
    ade4::add.scatter.eig(x$eig,x$nf,xax,yax,posi=posieig,ratio=1/4)
  } else {
    ade4::s.label(rbind.data.frame(coolig,coocol),clabel=0,cpoint=0,sub=sub,csub=csub)
    ade4::s.label(coolig,boxes=boxes,clabel=clab.row,add.plot=TRUE)
    ade4::s.label(coocol,boxes=boxes,clabel=clab.col,add.plot=TRUE)
    ade4::add.scatter.eig(x$eig,x$nf,xax,yax,posi=posieig,ratio=1/4)
  }
}
