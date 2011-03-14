mod <-
function(var) {
  dens<-density(var,na.rm=TRUE)
  dens$x[which(dens$y==max(dens$y))]
}

