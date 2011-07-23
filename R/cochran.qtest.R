cochran.qtest <-
function(resp,fact,block,alpha=0.05,p.method="fdr") {
  if (length(resp)!=length(fact)) {stop("'resp' and 'fact' lengths differ")}
  if (length(resp)!=length(block)) {stop("'resp' and 'block' lengths differ")}
  if (length(fact)!=length(block)) {stop("'fact' and 'block' lengths differ")}
  datas<-c(deparse(substitute(resp)),deparse(substitute(fact)),deparse(substitute(block)))
  if (!is.numeric(resp)) {resp<-as.numeric(as.character(resp))}
  if (!is.factor(fact)) {fact<-factor(fact)}
  if (!is.factor(block)) {block<-factor(block)}
  tab.length<-tapply(resp,list(block,fact),function(x) length(na.omit(x)))
  if (any(tab.length!=1)) {stop("there must be one observation by level of 'fact' in each block")}
  tab<-tapply(resp,list(block,fact),function(x) sum(x))
  k<-ncol(tab)
  b<-nrow(tab)
  X.j<-colSums(tab)
  Xi.<-rowSums(tab)
  N<-sum(X.j)
  Q<-k*(k-1)*sum((X.j-N/k)^2)/sum(Xi.*(k-Xi.))
  p<-min(pchisq(Q,k-1),pchisq(Q,k-1,lower.tail=FALSE))*2
  tab.test<-data.frame("Q"=Q,"df"=k-1,"p.value"=p,"signif"=psignif(p),row.names="")
  result=list(datas=datas,alpha=alpha,Q=Q,p.value=p,tab.test=tab.test)
  if (p<alpha) {
    comb<-combinations(nlevels(fact),2,levels(fact))
    chi2<-integer(nrow(comb))
    p.val<-integer(nrow(comb))
    for (i in 1:nrow(comb)) {
	signs<-integer(nrow(tab))
	for (j in 1:nrow(tab)) {
	  lev1<-resp[fact==comb[i,1] & block==levels(block)[j]]
	  lev2<-resp[fact==comb[i,2] & block==levels(block)[j]]
	  if (lev1>lev2) {
	    signs[j]<-"+"
	  } else if (lev1<lev2) {
	    signs[j]<-"-"
	  } else {
	    signs[j]<-NA
	  }
	}
	signs2<-na.omit(signs)
	p.val[i]<-min(pbinom(length(signs2[signs2=="+"]),length(signs2),0.5),
	  pbinom(length(signs2[signs2=="+"]),length(signs2),0.5,lower.tail=FALSE))*2
    }
    p.adj<-p.adjust(p.val,method=p.method)
    tab.comp<-data.frame("p.value"=p.adj,"signif"=psignif(p.adj),row.names=paste(comb[,1],"vs",comb[,2]))
    result$p.method<-p.method
    result$p.multcomp<-p.adj
    result$multcomp<-tab.comp
  }
  class(result)<-c("cochran.qtest","list")
  return(result)
}

