reg.intcomp <-
function(var,covar,fact,conf.level=0.95,theo=rep(0,nlevels(fact)),p.method="fdr"){
  if (length(var)!=length(fact)) {stop("'var' and 'fact' lengths differ")}
  if (length(var)!=length(covar)) {stop("'var' and 'covar' lengths differ")}
  if (length(covar)!=length(fact)) {stop("'covar' and 'fact' lengths differ")}
  if (is.character(fact) & !is.factor(fact)) {fact<-as.factor(fact)}
  nul<-as.numeric(row.names(table(c(which(is.na(var)),which(is.na(covar))))))
  var.2<-if(length(nul)>0) {var[-nul]} else {var}
  covar.2<-if(length(nul)>0) {covar[-nul]} else {covar}
  fact.2<-if(length(nul)>0) {fact[-nul]} else {fact}
  SPE<-integer(nlevels(fact.2))
  SCEx<-integer(nlevels(fact.2))
  SCEr<-integer(nlevels(fact.2))
  n<-integer(nlevels(fact.2))
  ord<-integer(nlevels(fact.2))
  ord.ci<-integer(nlevels(fact.2))
  t.obs<-integer(nlevels(fact.2))
  p<-integer(nlevels(fact.2))
  for (i in 1:nlevels(fact.2)) {
    SPE[i]<-sum((covar.2[fact.2==levels(fact.2)[i]]-mean(covar.2[fact.2==levels(fact.2)[i]]))*(var.2[fact.2==levels(fact.2)[i]]-mean(var.2[fact.2==levels(fact.2)[i]])))
    SCEx[i]<-sum((covar.2[fact.2==levels(fact.2)[i]]-mean(covar.2[fact.2==levels(fact.2)[i]]))^2)
    SCEr[i]<-sum(resid(lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]]))^2)
    ord[i]<-lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]])$coefficients[1]
    n[i]<-length(var.2[fact.2==levels(fact.2)[i]])
    varr<-SCEr[i]/(n[i]-2)
    ord.ci[i]<-qt((1+conf.level)/2,n[i]-2)*sqrt(varr*(1/n[i]+mean(covar.2[fact.2==levels(fact.2)[i]])^2/SCEx[i]))
    t.obs[i]<-abs(ord[i]-theo[i])/sqrt(varr*(1/n[i]+mean(covar.2[fact.2==levels(fact.2)[i]])^2/SCEx[i]))
    p[i]<-min(pt(t.obs[i],n[i]-2),pt(t.obs[i],n[i]-2,lower.tail=FALSE))*2
  }
  CMr=sum(SCEr)/(length(var.2)-2*nlevels(fact.2))
  dir.com<-sum(SPE)/sum(SCEx)
  dir.ci<-qt((1+conf.level)/2,length(var.2)-2*nlevels(fact.2))*sqrt(CMr/sum(SCEx))
  tab.dir<-data.frame("inf"=dir.com-dir.ci,"coeff"=dir.com,"sup"=dir.com+dir.ci,row.names="")
  tab.ord.ci<-data.frame("inf"=ord-ord.ci,"coeff"=ord,"sup"=ord+ord.ci,"theoretical"=theo,"t"=t.obs,"p.value"=p,
    "signif"=psignif(p),row.names=levels(fact.2))
  comb<-combinations(nlevels(fact.2),2,levels(fact.2))
  ddl<-integer(length(comb[,1]))
  t.obs.pair<-integer(length(comb[,1]))
  p.pair<-integer(length(comb[,1]))
  for (i in 1:length(comb[,1])) {
    ddl[i]<-n[levels(fact.2)==comb[i,1]]+n[levels(fact.2)==comb[i,2]]-4
    varr.com<-(SCEr[levels(fact.2)==comb[i,1]]+SCEr[levels(fact.2)==comb[i,2]])/ddl[i]
    t.obs.pair[i]<-abs(ord[levels(fact.2)==comb[i,1]]-ord[levels(fact.2)==comb[i,2]])/sqrt(varr.com*(1/n[levels(fact.2)==comb[i,1]]+mean(covar.2[levels(fact.2)==comb[i,1]])^2/SCEx[levels(fact.2)==comb[i,1]]+1/n[levels(fact.2)==comb[i,2]]+mean(covar.2[levels(fact.2)==comb[i,2]])^2/SCEx[levels(fact.2)==comb[i,2]]))
    p.pair[i]<-min(pt(t.obs.pair[i],ddl[i]),pt(t.obs.pair[i],ddl[i],lower.tail=FALSE))*2
  }
  p.pair.adj<-p.adjust(p.pair,method=p.method)
  tab.p.pair<-data.frame("df"=ddl,"t"=t.obs.pair,"p.value"=p.pair.adj,"signif"=psignif(p.pair.adj),
    row.names=paste(comb[,1],"vs",comb[,2]))
  result<-list(conf.level=conf.level,slope.comm=tab.dir,intercepts=ord,
    intercepts.ci=data.frame("inf"=ord-ord.ci,"coeff"=ord,"sup"=ord+ord.ci),intercepts.theo=theo,intercepts.t=t.obs,
    intercepts.p=p,intercepts.tab=tab.ord.ci,p.method=p.method,df.multcomp=ddl,t.multcomp=t.obs.pair,
    p.multcomp=p.pair.adj,multcomp=tab.p.pair)
  class(result)<-c("reg.intcomp","list")
  return(result)
}

