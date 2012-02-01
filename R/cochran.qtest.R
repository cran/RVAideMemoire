cochran.qtest <-
function(formula,data=NULL,alpha=0.05,p.method="fdr") {
  if (all.names(formula)[1]!="~" | all.names(formula)[3]!="|") {stop("incorrect 'formula'")}
  variables <- all.vars(formula)
  data.name <- paste(variables[1]," ~ ",variables[2],", block = ",variables[3],sep="")
  resp <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  fact <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  block <- if (is.null(data)) {get(variables[3],pos=environment(formula))}
    else {get(variables[3],pos=get(deparse(substitute(data))))}
  if (length(resp)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (length(resp)!=length(block)) {stop(paste("'",variables[1],"' and '",variables[3],"' lengths differ",sep=""))}
  if (length(fact)!=length(block)) {stop(paste("'",variables[2],"' and '",variables[3],"' lengths differ",sep=""))}
  if (!is.numeric(resp)) {resp <- as.numeric(as.character(resp))}
  if (!is.factor(fact)) {fact <- factor(fact)}
  if (!is.factor(block)) {block <- factor(block)}
  tab.length<-tapply(resp,list(block,fact),function(x) length(na.omit(x)))
  if (any(tab.length!=1)) {stop(paste("there must be 1 observation per level of '",variables[2],"' in each block",sep=""))}
  tab <- tapply(resp,list(block,fact),function(x) sum(x))
  k <- ncol(tab)
  b <- nrow(tab)
  X.j <- colSums(tab)
  Xi. <- rowSums(tab)
  N <- sum(X.j)
  Q <- k*(k-1)*sum((X.j-N/k)^2)/sum(Xi.*(k-Xi.))
  names(Q) <- "Q"
  p <- pchisq(Q,k-1,lower.tail=FALSE)
  names(p) <- NULL
  tab.test <- data.frame("Q"=Q,"Df"=k-1,"Pr(>Q)"=p," "=psignif(p),stringsAsFactors=FALSE,check.names=FALSE)
  result=list(data.name=data.name,statistic=Q,p.value=p,tab.test=tab.test,alpha=alpha)
  if (p<alpha) {
    fun.p <- function(i,j) {
	signs <- apply(tab[,c(i,j)],1,diff)
	binom.test(length(signs[signs>0]),length(signs[signs!=0]),0.5)$p.value
    }
    result$p.adjust.method <- p.method
    result$multcomp <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
  }
  class(result) <- c("cochran.qtest","list")
  return(result)
}
