bootstrap <-
function(x,fun,nrep=1000,conf.level=0.95,...){
  simul <- boot(x,fun,R=nrep,...)
  interval <- ci(simul$t,conf.level=conf.level)
  result <- list(conf.level=conf.level,rep=nrep,interval=interval)
  class(result) <- c("bootstrap","list")
  return(result)
}

