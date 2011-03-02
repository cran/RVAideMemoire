cramer.cor <-
function(var1,var2,rep=1000,conf.level=0.95,coeff.arr=4){
  if (length(var1)!=length(var2)) {stop("Les deux vecteurs n'ont pas la même taille !")}
  nul<-as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2<-if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2<-if(length(nul)>0) {var2[-nul]} else {var2}
  tab.cont<-table(var1,var2)
  v<-sqrt(as.numeric(suppressWarnings(chisq.test(tab.cont)$statistic))/(sum(tab.cont)*(min(dim(tab.cont))-1)))
  v.fun<-function(data,ind) {
    cont<-table(data[ind,1],data[ind,2])
    sqrt(as.numeric(suppressWarnings(chisq.test(cont)$statistic))/(sum(cont)*(min(dim(cont))-1)))
  }
  simul<-boot(data.frame(var1.2,var2.2),v.fun,R=rep)
  tri<-sort(simul$t)
  int<-(1-conf.level)/2
  if(rep*int<1) {
    int.inf<-ceiling(rep*int)
  } else {
    int.inf<-floor(rep*int)
  }
  int.sup<-ceiling(rep*(1-int))
  cat(paste("Coefficient d'association de Cramer\nV =",round(v,coeff.arr),"\n\n"))
  cat(paste("Intervalle de confiance à",100*conf.level,"%\n",round(tri[int.inf],coeff.arr),round(tri[int.sup],coeff.arr),"\n"))
}