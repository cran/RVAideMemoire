reg.ordcomp <-
function(var,covar,fact,conf.level=0.95,coeff.arr=4,theo=rep(0,nlevels(fact)),p.method="fdr",p.arr=4){
  if (length(var)!=length(fact) | length(fact)!=length(covar) | length(var)!=length(covar)) {stop("Les vecteurs n'ont pas la même taille !")}
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
  p<-round(p,p.arr)
  for (i in 1:length(p)) {
    if (p[i]<0.000001) {p[i]<-"< 10e-6"}
  }
  CMr=sum(SCEr)/(length(var.2)-2*nlevels(fact.2))
  dir.com<-sum(SPE)/sum(SCEx)
  dir.ci<-qt((1+conf.level)/2,length(var.2)-2*nlevels(fact.2))*sqrt(CMr/sum(SCEx))
  tab.dir<-data.frame(round(dir.com-dir.ci,coeff.arr),round(dir.com,coeff.arr),round(dir.com+dir.ci,coeff.arr),row.names="")
  names(tab.dir)<-c("inf","coeff","sup")
  tab.ord.ci<-data.frame(round(ord-ord.ci,coeff.arr),round(ord,coeff.arr),round(ord+ord.ci,coeff.arr),theo,round(t.obs,3),p,row.names=levels(fact.2))
  names(tab.ord.ci)<-c("inf","coeff","sup","théorique","t","p-value")
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
  p.pair.adj<-round(p.adjust(p.pair,method=p.method),p.arr)
  for (i in 1:length(p.pair.adj)) {
    if (p.pair.adj[i]<0.000001) {p.pair.adj[i]<-"< 10e-6"}
  }
  tab.p.pair<-data.frame(ddl,round(t.obs.pair,3),p.pair.adj,row.names=paste(comb[,1],"-",comb[,2]))
  names(tab.p.pair)<-c("ddl","t","p-value")
  cat(paste("Coefficient directeur commun et intervalle de confiance à",100*conf.level,"%\n"))
  print(tab.dir)
  cat(paste("\nOrdonnées à l'origine, intervalles de confiance à",100*conf.level,"%\net comparaison aux valeurs théoriques\n"))
  print(tab.ord.ci)
  cat(paste("\nComparaisons deux-à-deux des ordonnées - correction:",p.method,"\n"))
  print(tab.p.pair)
  cat("\n")
}

