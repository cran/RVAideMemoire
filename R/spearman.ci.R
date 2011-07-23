spearman.ci <-
function(var1,var2,rep=1000,conf.level=0.95){
  if (length(var1)!=length(var2)) {stop("'var1' and 'var2' lengths differ")}
  nul<-as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2<-if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2<-if(length(nul)>0) {var2[-nul]} else {var2}
  cor.fun<-function(data,ind) {
    as.numeric(suppressWarnings(cor.test(data[ind,1],data[ind,2])$estimate))
  }
  simul<-boot(data.frame(var1.2,var2.2),cor.fun,R=rep)
  tri<-sort(simul$t)
  int<-(1-conf.level)/2
  if(rep*int<1) {
    int.inf<-ceiling(rep*int)
  } else {
    int.inf<-floor(rep*int)
  }
  int.sup<-ceiling(rep*(1-int))
  result=list(conf.level=conf.level,rep=rep,
    coeff=as.numeric(suppressWarnings(cor.test(var1,var2,method="spearman")$estimate)),
    interval=c("Inf"=tri[int.inf],"Sup"=tri[int.sup]))
  class(result)<-c("spearman.ci","list")
  return(result)
}

