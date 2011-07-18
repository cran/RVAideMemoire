least.rect <-
function(x,y,conf.level=0.95,theo=1){
  if (length(x)!=length(y)) {stop("'x' and 'y' lengths differ")}
  nul<-as.numeric(row.names(table(c(which(is.na(x)),which(is.na(y))))))
  x.2<-if(length(nul)>0) {x[-nul]} else {x}
  y.2<-if(length(nul)>0) {y[-nul]} else {y}
  corr<-cor.test(x.2,y.2,method="pearson",conf.level=conf.level)
  r<-as.numeric(corr$estimate)
  k<-qt((1+conf.level)/2,length(x.2)-2)^2*(1-r^2)/(length(x.2)-2)
  b<-sd(y.2)/sd(x.2)*sign(cov(x.2,y.2))
  b.ci1<-b*sqrt(1+2*k-sqrt((1+2*k)^2-1))
  b.ci2<-b*sqrt(1+2*k+sqrt((1+2*k)^2-1))
  b.inf<-min(b.ci1,b.ci2)
  b.sup<-max(b.ci1,b.ci2)
  a<-mean(y.2)-b*mean(x.2)
  a.inf<-mean(y.2)-b.sup*mean(x.2)
  a.sup<-mean(y.2)-b.inf*mean(x.2)
  t.obs<-abs(b^2-theo^2)*sqrt(length(x.2)-2)/(2*b*theo*sqrt(1-r^2))
  p<-min(pt(t.obs,length(x.2)-2),pt(t.obs,length(x.2)-2,lower.tail=FALSE))*2
  conf.int<-matrix(c(a.inf,b.inf,a,b,a.sup,b.sup),nrow=2,dimnames=list(c("(Intercept)",deparse(substitute(x))),
    c("inf","coeff","sup")))
  conform<-data.frame("observed"=b,"theoretical"=theo,"df"=length(x.2)-2,"t"=t.obs,"p.value"=p,
    "signif"=psignif(p),row.names="")
  p.corr<-as.numeric(corr$p.value)
  corr.tab<-data.frame("inf"=as.numeric(corr$conf.int[1]),"r"=r,"sup"=as.numeric(corr$conf.int[2]),
    "df"=as.numeric(corr$parameter),"t"=as.numeric(corr$statistic),"p.value"=p.corr,"signif"=psignif(p.corr),
    row.names="")
  coeffs<-c(a,b)
  names(coeffs)<-c("(Intercept)",deparse(substitute(x)))
  fit<-a+b*x
  names(fit)<-1:length(x)
  res<-y-fit
  names(res)<-1:length(x)
  result=list(coefficients=coeffs,x=deparse(substitute(x)),y=deparse(substitute(y)),residuals=res,fitted.values=fit,
    conf.level=conf.level,conf.int=conf.int,slope.theo=theo,df.comp=length(x.2)-2,t.comp=t.obs,p.comp=p,
    model=data.frame(y,x),comp=conform,r=c("inf"=as.numeric(corr$conf.int[1]),"r"=r,
    "sup"=as.numeric(corr$conf.int[2])),r.df=as.numeric(corr$parameter),r.t=as.numeric(corr$statistic),
    r.p=p.corr,corr=corr.tab)
  class(result)<-c("least.rect","list")
  return(result)
}

