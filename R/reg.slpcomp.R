reg.slpcomp <-
function(formula,data=NULL,conf.level=0.95,p.method="fdr"){
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
  SCEx <- integer(nlevels(fact.2))
  SCEr <- integer(nlevels(fact.2))
  n <- integer(nlevels(fact.2))
  dir <- integer(nlevels(fact.2))
  dir.ci <- integer(nlevels(fact.2))
  for (i in 1:nlevels(fact.2)) {
    SCEx[i] <- sum((covar.2[fact.2==levels(fact.2)[i]]-mean(covar.2[fact.2==levels(fact.2)[i]]))^2)
    SCEr[i] <- sum(resid(lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]]))^2)
    n[i] <- length(var.2[fact.2==levels(fact.2)[i]])
    varr <- SCEr[i]/(n[i]-2)
    dir[i] <- lm(var.2[fact.2==levels(fact.2)[i]]~covar.2[fact.2==levels(fact.2)[i]])$coefficients[2]
    dir.ci[i] <- qt((1+conf.level)/2,n[i]-2)*sqrt(varr/SCEx[i])
  }
  tab.ci<-data.frame("inf"=dir-dir.ci,"coeff"=dir,"sup"=dir+dir.ci,row.names=levels(fact.2))
  fun.p <- function(i,j) {
    ddl <- n[i]+n[j]-4
    varr.com <- (SCEr[i]+SCEr[j])/ddl
    t.obs <- abs(dir[i]-dir[j])/sqrt(varr.com*(1/SCEx[i]+1/SCEx[j]))
    min(pt(t.obs,ddl),pt(t.obs,ddl,lower.tail=FALSE))*2
  }
  comp <- pairwise.table(fun.p,levels(fact.2),p.adjust.method=p.method)
  result <- list(data.name=dname,conf.level=conf.level,coeffs=dir,coeffs.tab=tab.ci,p.adjust.method=p.method,multcomp=comp)
  class(result) <- "reg.slpcomp"
  return(result)
}

print.reg.slpcomp <-
function(x,...) {
  cat("\n",x$data.name,"\n\n")
  cat(paste("Slopes and ",100*x$conf.level,"% confidence intervals\n",sep=""))
  print(x$coeffs.tab,digits=5)
  cat(paste("\nPairwise comparisons - correction: ",x$p.adjust.method,"\n"))
  print(x$multcomp,digits=5,na.print="-")
  cat("\n")
}

