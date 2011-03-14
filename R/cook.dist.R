cook.dist <-
function(model,avert=0.6){
  x<-model$model[,2]
  y<-model$model[,1]
  if (length(x)!=length(y)) {stop("Les deux vecteurs n'ont pas la même taille !")}
  seuil<-qf(0.5,2,length(y)-2)
  y.max<-max(max(ls.diag(lsfit(x,y))$cooks*1.125),(seuil*1.125))
  plot(ls.diag(lsfit(x,y))$cooks,type="h",ylim=c(0,y.max),main="",xlab="Numéro de l'individu",ylab="Distance de Cook")
  for (i in 1:length(x)) {
    if (ls.diag(lsfit(x,y))$cooks[i]>avert*seuil) {
	text(i,ls.diag(lsfit(x,y))$cooks[i]+0.05*y.max,as.character(i),cex=0.6,col="red")
    }
  }
  abline(h=seuil,lty=2,col="red")
  text(length(x)/5,seuil+0.05*y.max,"Distance théorique",font=3,cex=0.9,col="red")
}

