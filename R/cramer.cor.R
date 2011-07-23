cramer.cor <-
function(var1,var2,rep=1000,conf.level=0.95){
  if (length(var1)!=length(var2)) {stop("'var1' and 'var2' lengths differ")}
  if (!is.factor(var1)) {var1<-factor(var1)}
  if (!is.factor(var2)) {var2<-factor(var2)}
  nul<-as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2<-if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2<-if(length(nul)>0) {var2[-nul]} else {var2}
  tab.cont<-table(var1.2,var2.2)
  v<-sqrt(as.numeric(suppressWarnings(chisq.test(tab.cont)$statistic))/(sum(tab.cont)*(min(dim(tab.cont))-1)))
  v.fun<-function(data,ind) {
    cont<-table(data[ind,1],data[ind,2])
    sqrt(as.numeric(suppressWarnings(chisq.test(cont)$statistic))/(sum(cont)*(min(dim(cont))-1)))
  }
  simul<-boot(data.frame(var1.2,var2.2),v.fun,R=rep)
  simul$t<-simul$t[-which(!is.finite(simul$t))]
  tri<-sort(na.omit(simul$t))
  lg.tri<-length(tri)
  int<-(1-conf.level)/2
  if(lg.tri*int<1) {
    int.inf<-ceiling(lg.tri*int)
  } else {
    int.inf<-floor(lg.tri*int)
  }
  int.sup<-ceiling(lg.tri*(1-int))
  result=list(conf.level=conf.level,rep=rep,V=v,interval=c("Inf"=tri[int.inf],"Sup"=tri[int.sup]))
  class(result)<-c("cramer.cor","list")
  return(result)
}

