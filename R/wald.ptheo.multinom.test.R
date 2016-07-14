wald.ptheo.multinom.test <- function(x,p,p.method="fdr") {
  if (is.data.frame(x)) {x <- as.matrix(x)}
  if (is.matrix(x)) {
    if (!is.numeric(x)) {stop("incorrect 'x' format")}
    if (is.null(colnames(x))) {colnames(x) <- LETTERS[1:ncol(x)]}
    if (nrow(x)==1) {x <- factor(rep(colnames(x),x))}
  }
  if (is.matrix(x)) {
    lab <- colnames(x)
  } else if (is.factor(x) | is.character(x)) {
    x <- as.factor(x)
    lab <- levels(x)
  } else {stop("incorrect 'x' format")}
  if (sum(p)!=1) {stop("'p' must sum to 1")}
  dname <- paste(quote(x)," and ",quote(p),sep="")
  values <- prop.multinom(x)
  pval <- stat <- integer(length(lab))
  if (is.matrix(x)) {
    for (i in 1:length(lab)) {
	In <- x[,i]
	Out <- if (ncol(x)>2) {
	  if (nrow(x)==1) {
	    sum(x[,-i])
	  } else {
	    rowSums(x[,-i])
	  }
	} else {
	  if (nrow(x)==1) {
	    x[,-i]
	  } else {
	    t(t(x[,-i]))
	  }
	}
	test <- suppressWarnings(wald.ptheo.test(cbind(In,Out),p=p[i]))
	stat[i] <- test$statistic
	names(stat)[i] <- names(test$statistic)
	pval[i] <- test$p.value
    }
  } else {
    for (i in 1:length(lab)) {
	x2 <- relevel(factor(ifelse(as.numeric(x)==i,lab[i],"Other")),ref="Other")
	test <- suppressWarnings(wald.ptheo.test(x2,p=p[i]))
	stat[i] <- test$statistic
	names(stat)[i] <- names(test$statistic)
	pval[i] <- test$p.value
    }
  }
  p.adj <- p.adjust(pval,method=p.method)
  comp <- data.frame(" "=lab,"observed"=values$probs,"expected"=p,"statistic"=stat,"p-value"=p.adj,
    " "=.psignif(p.adj),stringsAsFactors=FALSE,check.names=FALSE)
  result <- list(method="Wald tests",data.name=dname,observed=values$probs,expected=p,p.adjust.method=p.method,
    statistic=stat,p.value2=p.adj,p.value=comp)
  class(result) <- "RV.multcomp"
  return(result)
}
