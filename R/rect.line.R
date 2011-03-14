rect.line <-
function(x,y){
  b<-sd(y,na.rm=TRUE)/sd(x,na.rm=TRUE)*sign(cov(x,y,use="complete.obs"))
  c(mean(y,na.rm=TRUE)-b*mean(x,na.rm=TRUE),b)
}

