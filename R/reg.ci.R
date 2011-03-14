reg.ci <-
function(model,conf.level=0.95,type=c("moy","ind"),...){
  if (length(x)!=length(y)) {stop("Les deux vecteurs n'ont pas la même taille !")}
  x<-model$model[,2]
  y<-model$model[,1]
  nul<-as.numeric(row.names(table(c(which(is.na(x)),which(is.na(y))))))
  x.2<-if(length(nul)>0) {x[-nul]} else {x}
  y.2<-if(length(nul)>0) {y[-nul]} else {y}
  sequence<-seq(min(x.2),max(x.2),abs(max(x.2)-min(x.2))/1000)
  pred<-predict(lm(y.2~x.2),list(x.2=sequence))
  n<-length(x.2)
  SCEr<-sum(resid(lm(y.2~x.2))^2)
  varr<-SCEr/(n-2)
  SCEx<-sum((x.2-mean(x.2))^2)
  if (length(type)>1) {type<-"moy"}
  if (type=="moy") {
    ci<-qt((1+conf.level)/2,n-2)*sqrt(varr*(1/n+(sequence-mean(x.2))^2/SCEx))
  } else if (type=="ind") {
    ci<-qt((1+conf.level)/2,n-2)*sqrt(varr*((n+1)/n+(sequence-mean(x.2))^2/SCEx))
  } else {stop("De quoi voulez-vous calculer l'intervalle de confiance ??")}
  lines(sequence,pred+ci,...)
  lines(sequence,pred-ci,...)
}

