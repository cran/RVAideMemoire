mod <-
function(x) {
  dens <- density(x,na.rm=TRUE)
  result <- dens$x[which(dens$y==max(dens$y))]
  return(result)
}

