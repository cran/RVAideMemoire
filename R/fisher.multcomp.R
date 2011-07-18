fisher.multcomp <-
function(tab.cont,p.method="fdr") {
  if (is.matrix(tab.cont)) {tab.cont<-as.table(tab.cont)}
  if (!is.table(tab.cont)) {stop("'tab.cont' is not a \"table\" object")}
  f1<-dimnames(tab.cont)[[1]]
  f2<-dimnames(tab.cont)[[2]]
  comb1<-combinations(length(f1),2,f1)
  comb2<-combinations(length(f2),2,f2)
  f1.lev=character(nrow(comb1)*nrow(comb2))
  f2.lev=character(nrow(comb1)*nrow(comb2))
  odr<-integer(nrow(comb1)*nrow(comb2))
  p<-integer(nrow(comb1)*nrow(comb2))
  for (i in 1:nrow(comb1)) {
    for (j in 1:nrow(comb2)) {
	tab<-tab.cont[c(comb1[i,1],comb1[i,2]),c(comb2[j,1],comb2[j,2])]
	f1.lev[i*nrow(comb2)-nrow(comb2)+j]<-paste(dimnames(tab)[[1]],collapse="-")
	f2.lev[i*nrow(comb2)-nrow(comb2)+j]<-paste(dimnames(tab)[[2]],collapse="-")
	test<-fisher.test(tab)
	odr[i*nrow(comb2)-nrow(comb2)+j]<-test$estimate
	p[i*nrow(comb2)-nrow(comb2)+j]<-test$p.value
    }
  }
  p.adj<-p.adjust(p,method=p.method)
  recap<-data.frame("rows"=f1.lev,"columns"=f2.lev,"odds.ratio"=odr,"p.value"=p.adj,"signif"=psignif(p.adj))
  lim.recap<-if (length(which(recap$p.value<0.1))>0) {
    recap[which(recap$p.value<0.1),]
  } else {"No comparison get a p-value < 0.1"}
  result<-list(p.method=p.method,levels=recap[,1:2],odds.ratio=odr,p.value=p.adj,total.recap=recap,
    limited.recap=lim.recap)
  class(result)<-c("fisher.multcomp","list")
  return(result)
}

