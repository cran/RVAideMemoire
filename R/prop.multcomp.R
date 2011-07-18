prop.multcomp <-
function(x,p,p.method="fdr") {
  obs<-integer(nrow(x))
  pval<-integer(nrow(x))
  for (i in 1:nrow(x)) {
    obs[i]<-x[i,1]/sum(x[i,])
    test<-binom.test(x[i,1],sum(x[i,]),p[i])
    pval[i]<-test$p.value
  }
  p.adj<-p.adjust(pval,method=p.method)
  comp<-data.frame("observed"=obs,"expected"=p,"p.value"=p.adj,"signif"=psignif(p.adj))
  if (!is.null(rownames(x))) {rownames(comp)<-rownames(x)}
  result<-list(observed=obs,expected=p,p.method=p.method,p.value=p.adj,comp=comp)
  class(result)<-c("prop.multcomp","list")
  return(result)
}

