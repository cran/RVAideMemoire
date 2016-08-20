# ade4 : dudi.coa

CDA.test <- function(X,fact,ncomp=NULL,...) {
  if (!is.data.frame(X)) {X <- as.data.frame(X)}
  if (!is.factor(fact)) {fact <- factor(fact)}
  if (is.null(ncomp)) {ncomp <- nlevels(fact)-1}
  CA <- dudi.coa(X,scannf=FALSE,nf=min(dim(X))-1)
  resp <- as.matrix(as.data.frame(CA$li[,1:ncomp]))
  if (ncol(resp)>1) {
    model <- manova(resp~fact)
    result <- summary.manova(model,...)
    attr(result$stats,"heading") <- c("Multivariate Analysis of Variance Table",
	paste(ncomp,"components"),paste("Response:",deparse(substitute(X))))
  } else {
    model <- lm(resp~fact)
    result <- anova(model)
    attr(result,"heading") <- c("Analysis of Variance Table",
	"1 component",paste("Response:",deparse(substitute(X))))
  }
  return(result)
}
