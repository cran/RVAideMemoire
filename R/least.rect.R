least.rect <-
function(x,y,conf.level=0.95,theo=1,coeff.arr=4,p.arr=4){
  if (length(x)!=length(y)) {stop("Les deux vecteurs n'ont pas la même taille !")}
  nul<-as.numeric(row.names(table(c(which(is.na(x)),which(is.na(y))))))
  x.2<-if(length(nul)>0) {x[-nul]} else {x}
  y.2<-if(length(nul)>0) {y[-nul]} else {y}
  corr<-cor.test(x.2,y.2,method="pearson",conf.level=conf.level)
  r<-as.numeric(corr$estimate)
  k<-qt((1+conf.level)/2,length(x.2)-2)^2*(1-r^2)/(length(x.2)-2)
  b<-sd(y.2)/sd(x.2)*sign(cov(x.2,y.2))
  b.inf<-b*sqrt(1+2*k-sqrt((1+2*k)^2-1))
  b.sup<-b*sqrt(1+2*k+sqrt((1+2*k)^2-1))
  a<-mean(y.2)-b*mean(x.2)
  a.inf<-mean(y.2)-b.sup*mean(x.2)
  a.sup<-mean(y.2)-b.inf*mean(x.2)
  t.obs<-abs(b^2-theo^2)*sqrt(length(x.2)-2)/(2*b*theo*sqrt(1-r^2))
  p<-min(pt(t.obs,length(x.2)-2),pt(t.obs,length(x.2)-2,lower.tail=FALSE))*2
  p<-round(p,p.arr)
  if (p<0.000001) {p<-"< 10e-6"}
  conf.int<-matrix(c(round(a.inf,coeff.arr),round(b.inf,coeff.arr),round(a,coeff.arr),round(b,coeff.arr),round(a.sup,coeff.arr),round(b.sup,coeff.arr)),nrow=2,dimnames=list(c("ordonnée à l'origine","coefficient directeur"),c("inf","coeff","sup")))
  conform<-data.frame(round(b,coeff.arr),theo,length(x.2)-2,round(t.obs,3),p,row.names="")
  names(conform)<-c("observé","théorique","ddl","t","p-value")
  p.corr<-round(as.numeric(corr$p.value),p.arr)
  if (p.corr<0.000001) {p.corr<-"< 10e-6"}
  corr<-data.frame(round(as.numeric(corr$conf.int[1]),coeff.arr),round(r,coeff.arr),round(as.numeric(corr$conf.int[2]),coeff.arr),as.numeric(corr$parameter),round(as.numeric(corr$statistic),3),p.corr,row.names="")
  names(corr)<-c("inf","r","sup","ddl","t","p-value")
  cat("\n        Régression linéaire simple au sens des moindres rectangles\n\n")
  cat(paste("Equation :",deparse(substitute(y)),"=",round(a,coeff.arr),"+",round(b,coeff.arr),deparse(substitute(x)),"\n\n"))
  cat("Intervalles de confiance à",100*conf.level,"% :\n")
  print(conf.int)
  cat("\nConformité de la pente à la valeur théorique",theo,":\n")
  print(conform)
  cat("\nCoefficient de corrélation linéaire de Pearson\n(intervalle de confiance à",100*conf.level,"%) :\n")
  print(corr)
  cat("\n")
}

