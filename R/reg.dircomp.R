reg.dircomp <-
function(var,covar,fact,conf.level=0.95,coeff.arr=4,p.method="fdr",p.arr=4){
  if (length(var)!=length(fact) | length(fact)!=length(covar) | length(var)!=length(covar)) {stop("Les vecteurs n'ont pas la même taille !")}
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
  p.adj<-round(p.adjust(p,method=p.method),p.arr)
  for (i in 1:length(p.adj)) {
    if (p.adj[i]<0.000001) {p.adj[i]<-"< 10e-6"}
  }
  tab.ci<-data.frame(inf=round(dir-dir.ci,coeff.arr),coeff=round(dir,coeff.arr),sup=round(dir+dir.ci,coeff.arr),row.names=levels(fact.2))
  tab.p<-data.frame(ddl,round(t.obs,3),p.adj,row.names=paste(comb[,1],"-",comb[,2]))
  names(tab.p)<-c("ddl","t","p-value")
  cat(paste("Coefficients directeurs et intervalles de confiance à",100*conf.level,"%\n"))
  print(tab.ci)
  cat(paste("\nComparaisons deux-à-deux des coefficients - correction:",p.method,"\n"))
  print(tab.p)
  cat("\n")
}

