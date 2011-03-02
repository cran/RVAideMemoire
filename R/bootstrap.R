bootstrap <-
function(vect,fun,rep=1000,conf.level=0.95,arr=4,...){
  simul<-boot(vect,fun,rep,...)
  tri<-sort(simul$t)
  int<-(1-conf.level)/2
  if(rep*int<1) {
    int.inf<-ceiling(rep*int)
  } else {
    int.inf<-floor(rep*int)
  }
  int.sup<-ceiling(rep*(1-int))
  cat(paste("Intervalle de confiance à",100*conf.level,"%\n",round(tri[int.inf],arr),round(tri[int.sup],arr),"\n"))
}

