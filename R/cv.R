cv <-
function(var,abs=TRUE,pc=TRUE) {
  coeff<-sd(var,na.rm=TRUE)/mean(var,na.rm=TRUE)
  if (abs==TRUE) {coeff<-abs(coeff)}
  if (pc==TRUE) {coeff<-100*coeff}
  coeff
}

