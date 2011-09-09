surv.multcomp <-
function(surv,fact,mat,strata=NULL,type=c("survreg","coxph"),distribution="exponential",
 p.method="fdr") {
  if (!is.Surv(surv)) {stop("'surv' is not a Surv() object")}
  if (nrow(rbind(surv))!=length(fact)) {stop("'surv' and 'fact' lengths differ")}
  if (!is.matrix(mat)) {stop("'mat' is not a \"matrix\" object")}
  if (!is.factor(fact)) {
    fact2<-as.factor(fact)
  } else {fact2<-fact}
  if (ncol(mat)!=nlevels(fact2)) {stop("incorrect 'mat' dimensions")}
  colnames(mat)<-levels(fact2)
  suppressWarnings(if (!type%in%c("survreg","coxph")) {stop("model type not recognized")})
  if (length(type)>1) {type<-"coxph"}
  liste.surv<-list()
  if (!is.null(strata)) {liste.stra<-list()}
  for (i in 1:nlevels(fact2)) {
    liste.surv[[i]]<-subset(surv,fact2==levels(fact2)[i])
    if (!is.null(strata)) {liste.stra[[i]]<-subset(strata,fact2==levels(fact2)[i])}
  }
  names(liste.surv)<-levels(fact2)
  if (!is.null(strata)) {names(liste.stra)<-levels(fact2)}
  comparisons<-character(nrow(mat))
  test<-integer(nrow(mat))
  p<-integer(nrow(mat))
  for (i in 1:nrow(mat)) {
    contrast<-mat[i,]
    num1<-which(contrast>0)
    num2<-which(contrast<0)
    ech1<-NULL
    ech2<-NULL
    if (!is.null(strata)) {
	stra1<-NULL
	stra2<-NULL
    }
    for (j in 1:length(liste.surv)) {
	if (any(names(num1)==names(liste.surv)[j])) {
	  ech1<-rbind(ech1,liste.surv[[j]])
	  if (!is.null(strata)) {stra1<-c(stra1,liste.stra[[j]])}
	}
	if (any(names(num2)==names(liste.surv)[j])) {
	  ech2<-rbind(ech2,liste.surv[[j]])
	  if (!is.null(strata)) {stra2<-c(stra2,liste.stra[[j]])}
	}
    }
    strata2<-if (!is.null(strata)) {
	c(stra1,stra2)
    } else {
	rep(NA,length(ech1)+length(ech2))
    }
    datas<-data.frame(fac=c(rep("Level1",dim(ech1)[1]),rep("Level2",dim(ech2)[1])),rbind(ech1,ech2),stra=strata2)
    if (ncol(datas)==4) {
	surv2<-Surv(datas$time,datas$status)
    } else if (ncol(datas)==5) {
	surv2<-Surv(datas$start,datas$stop,datas$status)
    }
    model<-if (!is.null(strata)) {
	if (type=="survreg") {
	  survreg(surv2~datas$fac+strata(datas$stra),dist=distribution)
	} else {
	  coxph(surv2~datas$fac+strata(datas$stra))
	}
    } else {
	if (type=="survreg") {
	  survreg(surv2~datas$fac,dist=distribution)
	} else {
	  coxph(surv2~datas$fac)
	}
    }
    test[i]<-if (type=="survreg") {
	summary(model)$chi
    } else {
	as.numeric(summary(model)$logtest[1])
    }
    p[i]<-if (type=="survreg") {
	1-pchisq(test[i],1)
    } else {
	as.numeric(summary(model)$logtest[3])
    }
    comparisons[i]<-paste(paste(colnames(mat)[which(contrast>0)],collapse="-"),"vs",
	paste(colnames(mat)[which(contrast<0)],collapse="-"))
  }
  p.adj<-p.adjust(p,method=p.method)
  comp<-data.frame("statistic"=test,"p.value"=p.adj,"signif"=psignif(p.adj),row.names=comparisons)
  model.txt<-if (!is.null(strata)) {
    if (type=="survreg") {
	paste("model: survreg(",deparse(substitute(surv))," ~ ",deparse(substitute(fact))," + strata(",
	  deparse(substitute(strata)),"), dist=\"",distribution,"\")",sep="")
    } else {
	paste("model: coxph(",deparse(substitute(surv))," ~ ",deparse(substitute(fact))," + strata(",
	  deparse(substitute(strata)),"))",sep="")
    }
  } else {
    if (type=="survreg") {
	paste("model: survreg(",deparse(substitute(surv))," ~ ",deparse(substitute(fact)),", dist=\"",distribution,
	  "\")",sep="")
    } else {
	paste("model: coxph(",deparse(substitute(surv))," ~ ",deparse(substitute(fact)),")",sep="")
    }  }
  result<-list(model=model.txt,statistics=test,p.method=p.method,p.value=p.adj,comparisons=comp)
  class(result)<-c("surv.multcomp","list")
  return(result)
}

