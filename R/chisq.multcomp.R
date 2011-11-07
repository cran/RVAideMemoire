chisq.multcomp <-
function(x,p.method="fdr") {
  names(x)<-LETTERS[1:length(x)]
  comb<-combinations(length(x),2,names(x))
  rows<-character(nrow(comb))
  chi2<-integer(nrow(comb))
  pval<-integer(nrow(comb))
  for (i in 1:nrow(comb)) {
    rows[i]<-paste(x[names(x)==comb[i,1]],"vs",x[names(x)==comb[i,2]])
    test<-suppressWarnings(chisq.test(c(x[names(x)==comb[i,1]],x[names(x)==comb[i,2]])))
    chi2[i]<-as.numeric(test$statistic)
    pval[i]<-as.numeric(test$p.value)
  }
  p.adj<-p.adjust(pval,method=p.method)
  tab.comp<-data.frame("p.value"=p.adj,"signif"=psignif(p.adj),row.names=rows)
  result<-list(p.method=p.method,chi2=chi2,p=p.adj,comp=tab.comp)
  class(result)<-c("chisq.multcomp","list")
  return(result)
}