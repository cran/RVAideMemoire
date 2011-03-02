cor.conf <-
function(var1,var2,theo,r.arr=4,p.arr=4) {
  nul<-as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2<-if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2<-if(length(nul)>0) {var2[-nul]} else {var2}
  r<-as.numeric(cor.test(var1.2,var2.2,method="pearson")$estimate)
  z<-0.5*log((1+r)/(1-r))
  zeta<-0.5*log((1+theo)/(1-theo))
  u.obs<-abs(z-zeta)*sqrt(length(var1.2)-3)
  p<-min(pnorm(u.obs,0,1),pnorm(u.obs,0,1,lower.tail=FALSE))*2
  p<-round(p,p.arr)
  if (p<0.000001) {p<-"< 10e-6"}
  conform<-data.frame(round(r,r.arr),theo,round(u.obs,3),p,row.names="")
  names(conform)<-c("observé","théorique","u","p-value")
  cat(paste("Conformité d'un coefficient de corrélation linéaire de Pearson\navec la valeur théorique",theo,"\n\n"))
  print(conform)
  cat("\n")
}

