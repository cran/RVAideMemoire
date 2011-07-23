reg.slpcomp <-
function(var,covar,fact,conf.level=0.95,p.method="fdr"){
  if (length(var)!=length(fact)) {stop("'var' and 'fact' lengths differ")}
  if (length(var)!=length(covar)) {stop("'var' and 'covar' lengths differ")}
  if (length(covar)!=length(fact)) {stop("'covar' and 'fact' lengths differ")}
  if (is.character(fact) & !is.factor(fact)) {fact<-as.factor(fact)}
  nul<-as.numeric(row.names(table(c(which(is.na(var)),which(is.na(covar))))))
  var.2<-if(length(nul)>0) {var[-nul]} else {var}
  covar.2<-if(length(nul)>0) {covar[-nul]} else {covar}
  fact.2<-if(length(nul)>0) {fact[-nul]} else {fact}
  SCEx<-integer(nlevels(fact.2))
  SCEr<-integer(nlevels(fact.2))
  n<-integer(nlevels(fact.2))
  dir<-integer(nlevels(fact.2))
  dir.ci<-integer(nlevels(fact.2))
  for (i in 1:nlevels(fact.2)) {
    SCEx[i]<-sum((covar.2[fact.2==levels(fact.2)[i]]-mean(covar.2[fact.2==levels(fact.2)[i]]))^2)
    SCEr[i]<-sum(resid(lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]]))^2)
    n[i]<-length(var.2[fact.2==levels(fact.2)[i]])
    varr<-SCEr[i]/(n[i]-2)
    dir[i]<-lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]])$coefficients[2]
    dir.ci[i]<-qt((1+conf.level)/2,n[i]-2)*sqrt(varr/SCEx[i])
  }
  comb<-combinations(nlevels(fact.2),2,levels(fact.2))
  ddl<-integer(length(comb[,1]))
  t.obs<-integer(length(comb[,1]))
  p<-integer(length(comb[,1]))
  for (i in 1:length(comb[,1])) {
    ddl[i]<-n[levels(fact.2)==comb[i,1]]+n[levels(fact.2)==comb[i,2]]-4
    varr.com<-(SCEr[levels(fact.2)==comb[i,1]]+SCEr[levels(fact.2)==comb[i,2]])/ddl[i]
    t.obs[i]<-abs(dir[levels(fact.2)==comb[i,1]]-dir[levels(fact.2)==comb[i,2]])/sqrt(varr.com*(1/SCEx[levels(fact.2)==comb[i,1]]+1/SCEx[levels(fact.2)==comb[i,2]]))
    p[i]<-min(pt(t.obs[i],ddl[i]),pt(t.obs[i],ddl[i],lower.tail=FALSE))*2
  }
  p.adj<-p.adjust(p,method=p.method)
  tab.ci<-data.frame("inf"=dir-dir.ci,"coeff"=dir,"sup"=dir+dir.ci,row.names=levels(fact.2))
  tab.p<-data.frame("df"=ddl,"t"=t.obs,"p.value"=p.adj,"signif"=psignif(p.adj),row.names=paste(comb[,1],"vs",comb[,2]))
  result<-list(conf.level=conf.level,coeffs=dir,coeffs.tab=tab.ci,p.method=p.method,df=ddl,t=t.obs,p.value=p.adj,
    multcomp=tab.p)
  class(result)<-c("reg.slpcomp","list")
  return(result)
}

