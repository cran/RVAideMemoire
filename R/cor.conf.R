cor.conf <-
function(var1,var2,theo) {
  if (length(var1)!=length(var2)) {stop("'var1' and 'var2' lengths differ")}
  if (theo<(-1) | theo>1) {stop("'theo' must be between -1 and 1")}
  nul<-as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2<-if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2<-if(length(nul)>0) {var2[-nul]} else {var2}
  r<-as.numeric(cor.test(var1.2,var2.2,method="pearson")$estimate)
  z<-0.5*log((1+r)/(1-r))
  zeta<-0.5*log((1+theo)/(1-theo))
  u.obs<-abs(z-zeta)*sqrt(length(var1.2)-3)
  p<-min(pnorm(u.obs,0,1),pnorm(u.obs,0,1,lower.tail=FALSE))*2
  conform<-data.frame("observed"=r,"theoretical"=theo,"u"=u.obs,"p.value"=p,"signif"=psignif(p),row.names="")
  result<-list(r.theo=theo,r.obs=r,u=u.obs,p=p,conform=conform)
  class(result)<-c("cor.conf","list")
  return(result)
}

