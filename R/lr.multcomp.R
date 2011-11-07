lr.multcomp <-
function(formula,data=NULL,conf.level=0.95) {
  if (all.names(formula)[1]!="~" | all.names(formula)[3]!="|") {stop("incorrect 'formula'")}
  variables<-all.vars(formula)
  y<-if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  x<-if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  fact<-if (is.null(data)) {get(variables[3],pos=environment(formula))}
    else {get(variables[3],pos=get(deparse(substitute(data))))}
  if (length(x)!=length(y)) {stop(paste("'",variables[2],"' and '",variables[1],"' lengths differ",sep=""))}
  if (length(x)!=length(fact)) {stop(paste("'",variables[2],"' and '",variables[3],"' lengths differ",sep=""))}
  if (length(y)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[3],"' lengths differ",sep=""))}
  if (is.character(fact) & !is.factor(fact)) {fact<-factor(fact)}
  nul<-as.numeric(row.names(table(c(which(is.na(y)),which(is.na(x))))))
  y2<-if(length(nul)>0) {y[-nul]} else {y}
  x2<-if(length(nul)>0) {x[-nul]} else {x}
  fact2<-if(length(nul)>0) {fact[-nul]} else {fact}
  intcpt<-matrix(ncol=3,nrow=nlevels(fact2),dimnames=list(levels(fact2),c("Inf","Intcpt","Sup")))
  slope<-matrix(ncol=3,nrow=nlevels(fact2),dimnames=list(levels(fact2),c("Inf","Slope","Sup")))
  for (i in 1:nlevels(fact2)) {
    y.temp<-y2[fact2==levels(fact2)[i]]
    x.temp<-x2[fact2==levels(fact2)[i]]
    corr<-cor.test(x.temp,y.temp,method="pearson",conf.level=conf.level)
    r<-as.numeric(corr$estimate)
    k<-qt((1+conf.level)/2,length(x.temp)-2)^2*(1-r^2)/(length(x.temp)-2)
    slope[i,2]<-(sd(y.temp)/sd(x.temp))*sign(cov(x.temp,y.temp))
    slope.ci1<-slope[i,2]*sqrt(1+2*k-sqrt((1+2*k)^2-1))
    slope.ci2<-slope[i,2]*sqrt(1+2*k+sqrt((1+2*k)^2-1))
    slope[i,1]<-min(slope.ci1,slope.ci2)
    slope[i,3]<-max(slope.ci1,slope.ci2)
    intcpt[i,2]<-mean(y.temp)-slope[i,2]*mean(x.temp)
    intcpt[i,1]<-mean(y.temp)-slope[i,3]*mean(x.temp)
    intcpt[i,3]<-mean(y.temp)-slope[i,1]*mean(x.temp)
  }
  comb<-combinations(nlevels(fact2),2,levels(fact2))
  comp<-data.frame("Slopes"=character(nrow(comb)),"Intercepts"=character(nrow(comb)),
    row.names=paste(comb[,1],"vs",comb[,2]))
  class(comp[,1])<-class(comp[,2])<-"character"
  for (i in 1:nrow(comb)) {
    lev1<-comb[i,1]
    lev2<-comb[i,2]
    if (slope[lev1,2]>slope[lev2,2]) {
	if (slope[lev1,1]>slope[lev2,3]) {comp[i,1]<-"unequal"} else {comp[i,1]<-"equal"}
    } else if (slope[lev1,2]<slope[lev2,2]) {
	if (slope[lev1,3]<slope[lev2,1]) {comp[i,1]<-"unequal"} else {comp[i,1]<-"equal"}
    } else {comp[i,1]<-"equal"}
    if (intcpt[lev1,2]>intcpt[lev2,2]) {
	if (intcpt[lev1,1]>intcpt[lev2,3]) {comp[i,2]<-"unequal"} else {comp[i,2]<-"equal"}
    } else if (intcpt[lev1,2]<intcpt[lev2,2]) {
	if (intcpt[lev1,3]<intcpt[lev2,1]) {comp[i,2]<-"unequal"} else {comp[i,2]<-"equal"}
    } else {comp[i,2]<-"equal"}
  }
  result<-list(data=variables,conf.level=conf.level,n.reg=nlevels(fact2),intercepts=intcpt,slopes=slope,comp=comp)
  class(result)<-c("lr.multcomp","list")
  return(result)
}

