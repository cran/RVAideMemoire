bootstrap <-
function(x,fun,rep=1000,conf.level=0.95,...){
  simul<-boot(x,fun,R=rep,...)
  tri<-sort(simul$t)
  int<-(1-conf.level)/2
  if(rep*int<1) {
    int.inf<-ceiling(rep*int)
  } else {
    int.inf<-floor(rep*int)
  }
  int.sup<-ceiling(rep*(1-int))
  result<-list(conf.level=conf.level,rep=rep,interval=c("Inf"=tri[int.inf],"Sup"=tri[int.sup]))
  class(result)<-c("bootstrap","list")
  return(result)
}

