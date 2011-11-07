cochran.qtest <-
function(formula,data=NULL,alpha=0.05,p.method="fdr") {
  if (all.names(formula)[1]!="~" | all.names(formula)[3]!="|") {stop("incorrect 'formula'")}
  variables<-all.vars(formula)
  resp<-if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  fact<-if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  block<-if (is.null(data)) {get(variables[3],pos=environment(formula))}
    else {get(variables[3],pos=get(deparse(substitute(data))))}
  if (length(resp)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (length(resp)!=length(block)) {stop(paste("'",variables[1],"' and '",variables[3],"' lengths differ",sep=""))}
  if (length(fact)!=length(block)) {stop(paste("'",variables[2],"' and '",variables[3],"' lengths differ",sep=""))}
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
  p<-pchisq(Q,k-1,lower.tail=FALSE)
  tab.test<-data.frame("Q"=Q,"df"=k-1,"p.value"=p,"signif"=psignif(p),row.names="")
  result=list(data=variables,alpha=alpha,Q=Q,p.value=p,tab.test=tab.test)
  if (p<alpha) {
    comb<-combinations(nlevels(fact),2,levels(fact))
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
	p.val[i]<-binom.test(length(signs2[signs2=="+"]),length(signs2),0.5)$p.value
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
