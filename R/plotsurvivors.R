plotsurvivors <-
function(x,status=rep(1,length(x))) {
  if (any(x==0)) {
    x<-x[-which(x==0)]
    status<-status[-which(x==0)]
  }
  if (any(status==0)) {
    x<-x[-which(status==0)]
    status<-status[-which(status==0)]
  }
  n<-length(x)
  time<-max(x)
  tri<-sort(x)
  alive<-integer(time+1)
  alive[1]<-n
  for (i in 2:length(alive)) {
    alive[i]<-alive[i-1]-length(which(tri==i-1))
  }
  suppressWarnings(plot(c(0,1:time),alive,pch=16,xlab="Time",ylab="Survivors (log scale)",log="y"))
  result=list(n=n,time=time,time.sort=tri,alive=alive)
  class(result)<-c("plotsurvivors","list")
  return(result)
}

