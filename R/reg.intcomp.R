reg.intcomp <-
function(formula,data,conf.level=0.95,theo=0,p.method="fdr"){
  if (missing(formula)) {stop("formula missing")}
  if ((length(formula)!=3) || (length(formula[[3]])!=3) || (formula[[3]][[1]]!=as.name("|")) ||
    (length(formula[[3]][[2]])!=1) || (length(formula[[3]][[3]])!=1)) {stop("incorrect specification for formula")}
  formula[[3]][[1]] <- as.name("+")
  m <- match.call()
  m$formula <- formula
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  m$conf.level <- m$theo <- m$p.method <- NULL
  mf <- eval(m,parent.frame())
  dname <- paste(names(mf)[1]," ~ ",names(mf)[2],", factor = ",names(mf)[3],sep="")
  var <- mf[,1]
  covar <- mf[,2]
  fact <- mf[,3]
  nul <- as.numeric(row.names(table(c(which(is.na(var)),which(is.na(covar))))))
  var.2 <- if(length(nul)>0) {var[-nul]} else {var}
  covar.2 <- if(length(nul)>0) {covar[-nul]} else {covar}
  fact.2 <- if(length(nul)>0) {fact[-nul]} else {fact}
  if (length(theo)==1) {theo <- rep(theo,nlevels(fact.2))}
  SPE <- integer(nlevels(fact.2))
  SCEx <- integer(nlevels(fact.2))
  SCEr <- integer(nlevels(fact.2))
  n <- integer(nlevels(fact.2))
  ord <- integer(nlevels(fact.2))
  ord.ci <- integer(nlevels(fact.2))
  t.obs <- integer(nlevels(fact.2))
  p <- integer(nlevels(fact.2))
  for (i in 1:nlevels(fact.2)) {
    SPE[i] <- sum((covar.2[fact.2==levels(fact.2)[i]]-mean(covar.2[fact.2==levels(fact.2)[i]]))*(var.2[fact.2==levels(fact.2)[i]]-mean(var.2[fact.2==levels(fact.2)[i]])))
    SCEx[i] <- sum((covar.2[fact.2==levels(fact.2)[i]]-mean(covar.2[fact.2==levels(fact.2)[i]]))^2)
    SCEr[i] <- sum(resid(lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]]))^2)
    ord[i] <- lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]])$coefficients[1]
    n[i] <- length(var.2[fact.2==levels(fact.2)[i]])
    varr <- SCEr[i]/(n[i]-2)
    ord.ci[i] <- qt((1+conf.level)/2,n[i]-2)*sqrt(varr*(1/n[i]+mean(covar.2[fact.2==levels(fact.2)[i]])^2/SCEx[i]))
    t.obs[i] <- abs(ord[i]-theo[i])/sqrt(varr*(1/n[i]+mean(covar.2[fact.2==levels(fact.2)[i]])^2/SCEx[i]))
    p[i] <- min(pt(t.obs[i],n[i]-2),pt(t.obs[i],n[i]-2,lower.tail=FALSE))*2
  }
  CMr <- sum(SCEr)/(length(var.2)-2*nlevels(fact.2))
  dir.com <- sum(SPE)/sum(SCEx)
  dir.ci <- qt((1+conf.level)/2,length(var.2)-2*nlevels(fact.2))*sqrt(CMr/sum(SCEx))
  tab.dir <- data.frame("inf"=dir.com-dir.ci,"coeff"=dir.com,"sup"=dir.com+dir.ci,row.names="")
  tab.ord.ci <- data.frame("inf"=ord-ord.ci,"coeff"=ord,"sup"=ord+ord.ci,"theo"=theo,"t"=t.obs,"Pr(>|t|)"=p,
    " "=.psignif(p),row.names=levels(fact.2),stringsAsFactors=FALSE,check.names=FALSE)
  fun.p <- function(i,j) {
    ddl <- n[i]+n[j]-4
    varr.com <- (SCEr[i]+SCEr[j])/ddl
    t.obs.pair <- abs(ord[i]-ord[j])/sqrt(varr.com*(1/n[i]+mean(covar.2[as.integer(fact.2)==i])^2/SCEx[i]+1/n[j]+
	mean(covar.2[as.integer(fact.2)==j])^2/SCEx[j]))
    min(pt(t.obs.pair,ddl),pt(t.obs.pair,ddl,lower.tail=FALSE))*2
  }
  comp <- pairwise.table(fun.p,levels(fact.2),p.adjust.method=p.method)
  result <- list(data.name=dname,conf.level=conf.level,slope.comm=tab.dir,intercepts=ord,
    intercepts.ci=data.frame("inf"=ord-ord.ci,"coeff"=ord,"sup"=ord+ord.ci),intercepts.theo=theo,intercepts.t=t.obs,
    intercepts.p=p,intercepts.tab=tab.ord.ci,p.adjust.method=p.method,multcomp=comp)
  class(result) <- "reg.intcomp"
  return(result)
}

print.reg.intcomp <-
function(x,...) {
  cat("\n",x$data.name,"\n\n")
  cat(paste("Common slope and ",100*x$conf.level,"% confidence interval\n",sep=""))
  print(x$slope.comm,digits=5)
  cat(paste("\nIntercepts, ",100*x$conf.level,"% confidence intervals and equality to given values\n",sep=""))
  print(x$intercepts.tab,digits=5)
  cat(paste("\nPairwise comparisons - correction:",x$p.adjust.method,"\n"))
  print(x$multcomp,digits=5,na.print="-")
  cat("\n")
}

