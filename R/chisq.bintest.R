chisq.bintest <-
function(formula,data=NULL,alpha=0.05,p.method="fdr") {
  if (all.names(formula)[1]!="~") {stop("incorrect 'formula'")}
  variables <- all.vars(formula)
  data.name <- paste(variables[1]," by ",variables[2],sep="")
  resp <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  fact <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  if (length(resp)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (!is.factor(resp)) {resp <- factor(resp)}
  if (!is.factor(fact)) {fact <- factor(fact)}
  tab.cont <- table(fact,relevel(resp,ref="1"))
  tab.cont.exp <- suppressWarnings(chisq.test(tab.cont))$expected
  cochran <- length(tab.cont)-ceiling(0.8*length(tab.cont))
  result <- list(data.name=data.name,alpha=alpha)
  if (length(which(tab.cont.exp<5))<=cochran | any(tab.cont.exp<1)) {
    result$test <- "Chi-squared"
    test <- suppressWarnings(chisq.test(tab.cont))
    result$statistic <- test$statistic
    result$parameter <- test$parameter
    result$p.value <- test$p.value
    result$method <- "        Pearson's Chi-squared test"
  } else {
    result$test <- "Fisher"
    test <- fisher.test(tab.cont)
    result$p.value <- test$p.value
    result$method <- "        Fisher's Exact Test for Count Data"
  }
  if (test$p.value<alpha) {
    result$p.adjust.method <- p.method
    if (any(tab.cont.exp<5)) {
	lignes <- combn(rownames(tab.cont), 2)
	p.no.adjust <- matrix(NA, nrow = nlevels(fact), ncol = nlevels(fact), 
	  dimnames = list(levels(fact), levels(fact)))
	for (i in 1:ncol(lignes)) {
	  tab.temp <- tab.cont[lignes[,i],]
	  p.no.adjust[lignes[2,i],lignes[1,i]] <- fisher.test(tab.temp)$p.value
	}
	p.adjust <- matrix(p.adjust(p.no.adjust, method = p.method), 
	  nrow = nrow(p.no.adjust), ncol = ncol(p.no.adjust), dimnames = dimnames(p.no.adjust))
	result$multcomp <- p.adjust[-1,-ncol(p.adjust)]
	result$multcomp.method <- paste("Pairwise comparisons by Fisher's exact test - correction: ",p.method,sep="")
    } else {
	result$multcomp <- suppressWarnings(pairwise.prop.test(tab.cont,p.adjust.method=p.method))$p.value
	result$multcomp.method <- paste("Pairwise comparisons by Chi-squared test - correction: ",p.method,sep="")
    }
  }
  class(result) <- c("chisq.bintest","list")
  return(result)
}
