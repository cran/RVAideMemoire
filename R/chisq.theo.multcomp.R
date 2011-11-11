chisq.theo.multcomp <-
function(x,p,p.method="fdr") {
  if (sum(p)!=1) {stop("sum of probabilities must be 1")}
  theo<-integer(length(x))
  chi2<-integer(length(x))
  pval<-integer(length(x))
  for (i in 1:length(x)) {
    test<-suppressWarnings(chisq.test(c(x[i],sum(x)-x[i]),p=c(p[i],1-p[i])))
    theo[i]<-as.numeric(test$expected[1])
    chi2[i]<-as.numeric(test$statistic)
    pval[i]<-as.numeric(test$p.value)
  }
  p.adj<-p.adjust(pval,method=p.method)
  comp<-data.frame("observed"=x,"expected"=theo,"chi2"=chi2,"p.value"=p.adj,"signif"=psignif(p.adj))
  result<-list(observed=x,expected=theo,p.method=p.method,chi2=chi2,p=p.adj,comp=comp)
  class(result)<-c("chisq.theo.multcomp","list")
  return(result)
}

