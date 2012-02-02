scat.mix.categorical <-
function(dudi.obj,xax=1,yax=2,csub=2,possub="topleft",...){
  def.par <- par(no.readonly=TRUE)
  on.exit(par(def.par))
  tabcomplet <- eval(as.list(dudi.obj$call)[[2]],sys.frame(0))
  indexation <- dudi.obj$index=="f"
  oritab <- tabcomplet[,indexation]
  nvar <- ncol(oritab)
  levels <- integer(ncol(tabcomplet))
  for (i in 1:ncol(tabcomplet)) {
    levels <- c(levels,length(levels(tabcomplet[,i])))
  }
  ncol <- (max(levels)%/%5)+1
  par(mfrow=n2mfrow(nvar))
  for(i in 1 :nvar) {
    s.class(dudi.obj$li,oritab[,i],xax=xax,yax=yax,clabel=1.5,sub=names(oritab[i]),csub=csub,possub=possub,
	col=rep(c('black','red','blue','green','orange'),ncol),cgrid=0,cstar=0)
  }
}

