tend.curve <-
function(x,y,...) {
  if (length(x)!=length(y)) {stop("Les deux vecteurs n'ont pas la même taille !")}
  nul<-as.numeric(row.names(table(c(which(is.na(x)),which(is.na(y))))))
  x.2<-if(length(nul)>0) {x[-nul]} else {x}
  y.2<-if(length(nul)>0) {y[-nul]} else {y}
  model<-gam(y.2~s(x.2))
  sequence<-seq(min(x.2),max(x.2),abs(max(x.2)-min(x.2))/1000)
  pred<-predict(model,list(x.2=sequence))
  lines(sequence,pred,...)
}

