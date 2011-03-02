cor.2comp <-
function(var1,var2,var3,var4,alpha=0.05,conf.level=0.95,r.arr=4,theo=0,p.arr=4){
  nul1<-as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2<-if(length(nul1)>0) {var1[-nul1]} else {var1}
  var2.2<-if(length(nul1)>0) {var2[-nul1]} else {var2}
  nul2<-as.numeric(row.names(table(c(which(is.na(var3)),which(is.na(var4))))))
  var3.2<-if(length(nul2)>0) {var3[-nul2]} else {var3}
  var4.2<-if(length(nul2)>0) {var4[-nul2]} else {var4}
  r1<-as.numeric(cor.test(var1.2,var2.2,method="pearson")$estimate)
  r2<-as.numeric(cor.test(var3.2,var4.2,method="pearson")$estimate)
  z1<-0.5*log((1+r1)/(1-r1))
  z2<-0.5*log((1+r2)/(1-r2))
  u.obs<-abs(z1-z2)/sqrt(1/(length(var1.2)-3)+1/(length(var3.2)-3))
  p<-min(pnorm(u.obs,0,1),pnorm(u.obs,0,1,lower.tail=FALSE))*2
  p<-round(p,p.arr)
  if (p<0.000001) {p<-"< 10e-6"}
  tab<-data.frame(round(r1,r.arr),round(r2,r.arr),round(u.obs,3),p,row.names="")
  names(tab)<-c("r1","r2","u","p-value")
  if (p>alpha){
    z.moy<-sum((length(var1.2)-3)*z1,(length(var3.2)-3)*z2)/sum(length(var1.2)-3,length(var3.2)-3)
    r.com<-(exp(2*z.moy)-1)/(exp(2*z.moy)+1)
    z.moy.inf<-z.moy-qnorm((1+conf.level)/2,0,1)/sqrt(sum(length(var1.2),length(var3.2))-6)
    z.moy.sup<-z.moy+qnorm((1+conf.level)/2,0,1)/sqrt(sum(length(var1.2),length(var3.2))-6)
    r.com.inf<-(exp(2*z.moy.inf)-1)/(exp(2*z.moy.inf)+1)
    r.com.sup<-(exp(2*z.moy.sup)-1)/(exp(2*z.moy.sup)+1)
    zeta<-0.5*log((1+theo)/(1-theo))
    u.obs.com<-abs(z.moy-zeta)*sqrt(sum(length(var1.2),length(var3.2))-6)
    p.com<-min(pnorm(u.obs.com,0,1),pnorm(u.obs.com,0,1,lower.tail=FALSE))*2
    p.com<-round(p.com,p.arr)
    if (p.com<0.000001) {p.com<-"< 10e-6"}
    tab.com<-data.frame(round(r.com.inf,r.arr),round(r.com,r.arr),round(r.com.sup,r.arr),theo,round(u.obs.com,3),p.com,row.names="")
    names(tab.com)<-c("inf","r","sup","théorique","u","p-value")
  }
  cat("\n Comparaison de 2 coefficients de corrélation linéaire de Pearson\n\n")
  print(tab)
  cat("\n")
  if (p>alpha){
    cat(paste("Coefficient de corrélation commun, intervalle de confiance à",100*conf.level,"%\net test de conformité avec la valeur",theo,":\n"))
    print(tab.com)
    cat("\n")
  }
}

