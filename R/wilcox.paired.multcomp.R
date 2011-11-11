wilcox.paired.multcomp <-
function(formula,data=NULL,p.method="fdr") {
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
  result<-list(data=variables,p.method=p.method,V=V,p.value=p.adj,comp=tab.comp)
  class(result)<-c("wilcox.paired.multcomp","list")
  return(result)
}

