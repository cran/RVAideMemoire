cor.multcomp <-
function(var1,var2,fact,alpha=0.05,conf.level=0.95,r.arr=4,theo=0,p.method="fdr",p.arr=4){
  if (length(var1)!=length(var2) | length(var2)!=length(fact) | length(var1)!=length(fact)) {stop("Les vecteurs n'ont pas la même taille !")}
  nul<-as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2<-if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2<-if(length(nul)>0) {var2[-nul]} else {var2}
  fact.2<-if(length(nul)>0) {fact[-nul]} else {fact}
  n<-integer(nlevels(fact.2))
  r<-integer(nlevels(fact.2))
  z<-integer(nlevels(fact.2))
  r.ar<-list(NULL)
  for (i in 1:nlevels(fact.2)) {
    n[i]<-length(var1.2[fact.2==levels(fact.2)[i]])
    r[i]<-as.numeric(cor.test(var1.2[fact.2==levels(fact.2)[i]],var2.2[fact.2==levels(fact.2)[i]])$estimate)
    r.ar[[i]]<-round(r[i],r.arr)
    z[i]<-0.5*log((1+r[i])/(1-r[i]))
  }
  z.moy<-sum((n-3)*z)/sum(n-3)
  chi2.obs<-sum((n-3)*(z-z.moy)^2)
  p<-min(pchisq(chi2.obs,nlevels(fact)-1),pchisq(chi2.obs,nlevels(fact)-1,lower.tail=FALSE))*2
  p<-round(p,p.arr)
  if (p<0.000001) {p<-"< 10e-6"}
  tab<-data.frame(r.ar,round(chi2.obs,3),p,row.names="")
  names(tab)<-c(levels(fact.2),"chi2","p-value")
  if (p>alpha) {
    r.com<-(exp(2*z.moy)-1)/(exp(2*z.moy)+1)
    z.moy.inf<-z.moy-qnorm((1+conf.level)/2,0,1)/sqrt(sum(n)-3*nlevels(fact))
    z.moy.sup<-z.moy+qnorm((1+conf.level)/2,0,1)/sqrt(sum(n)-3*nlevels(fact))
    r.com.inf<-(exp(2*z.moy.inf)-1)/(exp(2*z.moy.inf)+1)
    r.com.sup<-(exp(2*z.moy.sup)-1)/(exp(2*z.moy.sup)+1)
    zeta<-0.5*log((1+theo)/(1-theo))
    u.obs.com<-abs(z.moy-zeta)*sqrt(sum(n)-3*nlevels(fact))
    p.com<-min(pnorm(u.obs.com,0,1),pnorm(u.obs.com,0,1,lower.tail=FALSE))*2
    p.com<-round(p.com,p.arr)
    if (p.com<0.000001) {p.com<-"< 10e-6"}
    tab.com<-data.frame(round(r.com.inf,r.arr),round(r.com,r.arr),round(r.com.sup,r.arr),theo,round(u.obs.com,3),p.com,row.names="")
    names(tab.com)<-c("inf","r","sup","théorique","u","p-value")
  }
  if (p<alpha & nlevels(fact.2)>2) {
    comb<-combinations(nlevels(fact.2),2,levels(fact.2))
    u.obs.pair<-integer(length(comb[,1]))
    p.pair<-integer(length(comb[,1]))
    for (i in 1:length(comb[,1])) {
	u.obs.pair[i]<-abs(z[levels(fact.2)==comb[i,1]]-z[levels(fact.2)==comb[i,2]])/sqrt(1/(n[levels(fact.2)==comb[i,1]]-3)+1/(n[levels(fact.2)==comb[i,2]]-3))
	p.pair[i]<-min(pnorm(u.obs.pair[i],0,1),pnorm(u.obs.pair[i],0,1,lower.tail=FALSE))*2
    }
    p.adj.pair<-round(p.adjust(p.pair,method=p.method),p.arr)
    for (i in length(p.adj.pair)) {
	if (p.adj.pair[i]<0.000001) {p.adj.pair[i]<-"< 10e-6"}
    }
    tab.pair<-data.frame(p.adj.pair,row.names=paste(comb[,1],"-",comb[,2]))
    names(tab.pair)<-"p-value"
  }
  cat(paste("\n Comparaison de",length(levels(fact.2)),"coefficients de corrélation linéaire de Pearson\n\n"))
  print(tab)
  cat("\n")
  if (p>alpha){
    cat(paste("Coefficient de corrélation commun, intervalle de confiance à",100*conf.level,"%\net test de conformité avec la valeur",theo,":\n"))
    print(tab.com)
    cat("\n")
  }
  if (p<alpha & nlevels(fact)>2) {
    cat(paste("Comparaisons deux-à-deux des coefficients - correction :",p.method,"\n"))
    print(tab.pair)
    cat("\n")
  }
}

