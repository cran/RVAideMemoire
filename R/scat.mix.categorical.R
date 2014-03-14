scat.mix.categorical <-
function(dudi.obj,xax=1,yax=2,csub=2,possub="topleft",...){
  def.par <- par(no.readonly=TRUE)
  on.exit(par(def.par))
  tabcomplet <- eval(as.list(dudi.obj$call)[[2]],sys.frame(0))
  indexation <- dudi.obj$index=="f"
  oritab <- as.data.frame(tabcomplet[,indexation])
  colnames(oritab) <- colnames(tabcomplet)[indexation]
  nvar <- ncol(oritab)
  lev <- integer(ncol(tabcomplet))
  for (i in 1:ncol(tabcomplet)) {
    lev[i] <- nlevels(tabcomplet[,i])
  }
  par(mfrow=n2mfrow(nvar))
  for(i in 1:nvar) {
    ade4::s.class(dudi.obj$li,oritab[,i],xax=xax,yax=yax,clabel=1.5,sub=names(oritab[i]),csub=csub,possub=possub,
	col=rainbow(nlevels(oritab[,i])),cgrid=0,cstar=0,...)
  }
}
