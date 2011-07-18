cor.2comp <-
function(var1,var2,var3,var4,alpha=0.05,conf.level=0.95,theo=0){
  if (length(var1)!=length(var2)) {stop("'var1' and 'var2' lengths differ")}
  if (length(var3)!=length(var4)) {stop("'var3' and 'var4' lengths differ")}
  if (theo<(-1) | theo>1) {stop("'theo' must be between -1 and 1")}
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
  tab<-data.frame("r1"=r1,"r2"=r2,"u"=u.obs,"p.value"=p,"signif"=psignif(p),row.names="")
  result<-list(conf.level=conf.level,alpha=alpha,coeffs=c(r1,r2),u.comp=u.obs,p.comp=p,comp=tab)
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
    tab.com<-data.frame("inf"=r.com.inf,"r"=r.com,"sup"=r.com.sup,"theoretical"=theo,"u"=u.obs.com,"p.value"=p.com,
	"signif"=psignif(p.com),row.names="")
    result$r.comm<-c("inf"=r.com.inf,"r"=r.com,"sup"=r.com.sup)
    result$r.theo<-theo
    result$u.comm<-u.obs.com
    result$p.comm<-p.com
    result$comm<-tab.com
  }
  class(result)<-c("cor.2comp","list")
  return(result)
}

