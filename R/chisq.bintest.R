chisq.bintest <-
function(formula,data,alpha=0.05,p.method="fdr") {
  if (missing(formula)||(length(formula)!=3)) {stop("missing or incorrect formula")}
  m <- match.call()
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  m$alpha <- m$p.method <- NULL
  mf <- eval(m,parent.frame())
  mf <- droplevels(mf[complete.cases(mf),])
  dname <- paste(names(mf)[1],paste(names(mf)[2:ncol(mf)],collapse=":"),sep=" by ")
  resp.mf <- mf[,1]
  resp <- factor(as.numeric(factor(resp.mf))-1)
  if (nlevels(resp)!=2) {stop(paste(names(mf)[1],"is not a binary variable"))}
  resp.num <- as.numeric(as.character(resp))
  fact <- interaction(mf[,2:ncol(mf)],sep=":")
  proba <- tapply(resp.num,fact,mean)
  names(proba) <- paste("proba in group ",levels(fact),sep="")
  tab.cont <- table(fact,relevel(resp,ref="1"))
  tab.cont.exp <- suppressWarnings(chisq.test(tab.cont))$expected
  cochran <- length(tab.cont)-ceiling(0.8*length(tab.cont))
  nval <- 0
  names(nval) <- "difference in probabilities"
  result <- list(data.name=dname,alternative="two.sided",null.value=nval,estimate=proba,alpha=alpha)
  if (length(which(tab.cont.exp<5))<=cochran | any(tab.cont.exp<1)) {
    test <- suppressWarnings(chisq.test(tab.cont))
    result$statistic <- test$statistic
    result$parameter <- test$parameter
    result$p.value <- test$p.value
    result$method.test <- "Pearson's Chi-squared test"
  } else {
    test <- fisher.test(tab.cont)
    result$p.value <- test$p.value
    result$method.test <- "Fisher's Exact Test for Count Data"
  }
  if (test$p.value<alpha) {
    result$p.adjust.method <- p.method
    if (any(tab.cont.exp<5)) {
	lignes <- combn(rownames(tab.cont), 2)
	p.no.adjust <- matrix(NA,nrow=nlevels(fact),ncol=nlevels(fact),dimnames=list(levels(fact),levels(fact)))
	for (i in 1:ncol(lignes)) {
	  tab.temp <- tab.cont[lignes[,i],]
	  p.no.adjust[lignes[2,i],lignes[1,i]] <- fisher.test(tab.temp)$p.value
	}
	p.adjust <- matrix(p.adjust(p.no.adjust,method=p.method),nrow=nrow(p.no.adjust),ncol=ncol(p.no.adjust),
	  dimnames = dimnames(p.no.adjust))
	result$p.value.multcomp <- p.adjust[-1,-ncol(p.adjust)]
	result$method.multcomp <- "Fisher's exact test for count data"
    } else {
	result$p.value.multcomp <- suppressWarnings(pairwise.prop.test(tab.cont,p.adjust.method=p.method))$p.value
	result$method.multcomp <- "Pearson's Chi-squared test"
    }
  }
  class(result) <- "RVtest"
  return(result)
}
