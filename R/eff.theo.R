eff.theo <-
function(data,p.theo,graphe=FALSE){
  if (length(p.theo)!=length(data[,1])){stop("Il n'y a pas le même nombre de proportions théoriques que de populations !")}
  n<-integer(length(data[,1]))
  for (i in 1:length(data[,1])) {n[i]=sum(data[i,])}
  n.theo1<-n*p.theo
  n.theo2<-n*(1-p.theo)
  n.theo.mat<-matrix(c(n.theo1,n.theo2),nrow=length(data[,1]),dimnames=list(rownames(data),colnames(data)))
  cochran.max<-ceiling(0.8*length(data))
  cochran.min<-length(data)-cochran.max
  cat('Effectifs théoriques\n\n')
  print(n.theo.mat)
  cat(paste("\nRègle de Cochran :",cochran.min,"case(s) maximum peu(ven)t être < ou = à 5\n"))
  if (graphe==TRUE) {mosaicplot(t(n.theo.mat),main="Distribution théorique",col=TRUE)}
}

