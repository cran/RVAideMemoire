wilcox.paired.multcomp <-
function(resp,fact,block,p.method="fdr") {
  if (length(resp)!=length(fact)) {stop("'resp' and 'fact' lengths differ")}
  if (length(resp)!=length(block)) {stop("'resp' and 'block' lengths differ")}
  if (length(fact)!=length(block)) {stop("'fact' and 'block' lengths differ")}
  datas<-c(deparse(substitute(resp)),deparse(substitute(fact)),deparse(substitute(block)))
  if (!is.factor(fact)) {fact<-factor(fact)}
  if (!is.factor(block)) {block<-factor(block)}
  comb<-combinations(nlevels(fact),2,levels(fact))
  V<-integer(nrow(comb))
  p<-integer(nrow(comb))
  for (i in 1:nrow(comb)) {
    vect1<-integer(nlevels(block))
    vect2<-integer(nlevels(block))
    for (j in 1:nlevels(block)) {
	vect1[j]<-resp[fact==comb[i,1] & block==levels(block)[j]]
	vect2[j]<-resp[fact==comb[i,2] & block==levels(block)[j]]
    }
    test<-suppressWarnings(wilcox.test(vect1,vect2,paired=TRUE))
    V[i]<-as.numeric(test$statistic)
    p[i]<-as.numeric(test$p.value)
  }
  p.adj<-p.adjust(p,method=p.method)
  tab.comp<-data.frame("V"=V,"p.value"=p.adj,"signif"=psignif(p.adj),row.names=paste(comb[,1],"vs",comb[,2]))
  result<-list(datas=datas,p.method=p.method,V=V,p.value=p.adj,comp=tab.comp)
  class(result)<-c("wilcox.paired.multcomp","list")
  return(result)
}

