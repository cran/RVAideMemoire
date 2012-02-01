cor.multcomp <-
function(var1,var2,fact,alpha=0.05,conf.level=0.95,theo=0,p.method="fdr"){
  if (length(var1)!=length(var2)) {stop("'var1' and 'var2' lengths differ")}
  if (length(var1)!=length(fact)) {stop("'var1' and 'fact' lengths differ")}
  if (length(var2)!=length(fact)) {stop("'var2' and 'fact' lengths differ")}
  if (theo<(-1) | theo>1) {stop("'theo' must be between -1 and 1")}
  if (!is.factor(fact)) {fact<-as.factor(fact)}
  nul <- as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2 <- if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2 <- if(length(nul)>0) {var2[-nul]} else {var2}
  fact.2 <- if(length(nul)>0) {fact[-nul]} else {fact}
  n <- integer(nlevels(fact.2))
  r <- integer(nlevels(fact.2))
  z <- integer(nlevels(fact.2))
  for (i in 1:nlevels(fact.2)) {
    n[i] <- length(var1.2[fact.2==levels(fact.2)[i]])
    r[i] <- as.numeric(cor.test(var1.2[fact.2==levels(fact.2)[i]],var2.2[fact.2==levels(fact.2)[i]])$estimate)
    z[i] <- 0.5*log((1+r[i])/(1-r[i]))
  }
  z.moy <- sum((n-3)*z)/sum(n-3)
  chi2.obs <- sum((n-3)*(z-z.moy)^2)
  names(chi2.obs) <- "Chi"
  p <- pchisq(chi2.obs,nlevels(fact)-1,lower.tail=FALSE)
  tab <- data.frame(t(r),chi2.obs,p,psignif(p))
  names(tab) <- c(levels(fact.2),"Chi","Pr(>Chi)"," ")
  result <- list(levels=nlevels(fact.2),conf.level=conf.level,alpha=alpha,coeffs=r,statistic.comp=chi2.obs,p.value.comp=p,comp=tab)
  names(result$coeffs) <- levels(fact.2)
  if (p>alpha) {
    r.com <- (exp(2*z.moy)-1)/(exp(2*z.moy)+1)
    z.moy.inf <- z.moy-qnorm((1+conf.level)/2,0,1)/sqrt(sum(n)-3*nlevels(fact))
    z.moy.sup <- z.moy+qnorm((1+conf.level)/2,0,1)/sqrt(sum(n)-3*nlevels(fact))
    r.com.inf <- (exp(2*z.moy.inf)-1)/(exp(2*z.moy.inf)+1)
    r.com.sup <- (exp(2*z.moy.sup)-1)/(exp(2*z.moy.sup)+1)
    zeta <- 0.5*log((1+theo)/(1-theo))
    u.obs.com <- abs(z.moy-zeta)*sqrt(sum(n)-3*nlevels(fact))
    names(u.obs.com) <- "u"
    p.com <- min(pnorm(u.obs.com,0,1),pnorm(u.obs.com,0,1,lower.tail=FALSE))*2
    tab.com<-data.frame("inf"=r.com.inf,"r"=r.com,"sup"=r.com.sup,"theoretical"=theo,"u"=u.obs.com,"Pr(>|u|)"=p.com,
	" "=psignif(p.com),stringsAsFactors=FALSE,check.names=FALSE)
    result$r.comm <- c("inf"=r.com.inf,"r"=r.com,"r.sup"=r.com.sup)
    result$r.theo <- theo
    result$statistic.comm <- u.obs.com
    result$p.value.comm <- p.com
    result$comm <- tab.com
  }
  if (p<alpha & nlevels(fact.2)>2) {
    fun.p <- function(i,j) {
	u <- abs(z[i]-z[j])/sqrt(1/(n[i]-3)+1/(n[j]-3))
	min(pnorm(u,0,1),pnorm(u,0,1,lower.tail=FALSE))*2
    }
    result$p.adjust.method <- p.method
    result$p.multcomp <- pairwise.table(fun.p,levels(fact.2),p.adjust.method=p.method)
  }
  class(result) <- c("cor.multcomp","list")
  return(result)
}

