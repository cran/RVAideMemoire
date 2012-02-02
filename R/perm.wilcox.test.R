perm.wilcox.test <-
function(formula,data=NULL,alternative=c("two.sided","less","greater"),paired=FALSE,nperm=999) {
  if (all.names(formula)[1]!="~") {stop("incorrect 'formula'")}
  variables<-all.vars(formula)
  resp <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  fact <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  if (length(resp)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (!is.numeric(resp)) {resp <- as.numeric(as.character(resp))}
  if (!is.factor(fact)) {fact <- factor(fact)}
  if (nlevels(fact)!=2) {stop(paste(variables[2]," is not a 2-levels factor",sep=""))}
  if (paired==TRUE & diff(tapply(resp,fact,length))!=0) {stop(paste("'",levels(fact)[1],"' and '",levels(fact)[2],"' lengths differ",sep=""))}
  if (length(alternative)>1) {alternative <- "two.sided"}
  data.name <- paste("data: ",variables[1]," by ",variables[2],sep="")
  method <- NULL
  W.ref <- suppressWarnings(wilcox.test(resp~fact,alternative=alternative,paired=paired))$statistic
  W.perm <- numeric(nperm+1)
  W.perm[1] <- W.ref
  if (!paired) {
    method <- "     Permutational Wilcoxon rank sum test"
    for(i in 1:nperm) {
	W.perm[i+1] <- suppressWarnings(wilcox.test(sample(resp)~fact,alternative=alternative,paired=FALSE))$statistic
    }
  } else {
    method <- "     Permutational Wilcoxon signed rank test"
    resp2 <- cbind(resp[fact==levels(fact)[1]],resp[fact==levels(fact)[2]])
    for (i in 1:nperm) {
	resp.perm <- t(apply(resp2,1,sample))
	W.perm[i+1] <- suppressWarnings(wilcox.test(resp.perm[,1],resp.perm[,2],alternative=alternative,paired=TRUE))$statistic
    }
  }
  pvalue <- NULL
  H1 <- NULL
  if (alternative=="two.sided") {
    pvalue <- min(length(which(W.perm <= W.ref)),length(which(W.perm >= W.ref)))*2/(nperm+1)
    H1 <- "alternative hypothesis: true location shift is not equal to 0"
  }
  if (alternative=="less") {
    pvalue <- length(which(W.perm <= W.ref))/(nperm+1)
    H1 <- "alternative hypothesis: true location shift is less than 0"
    }
  if (alternative=="greater") {
    pvalue <- length(which(W.perm >= W.ref))/(nperm+1)
    H1 <- "alternative hypothesis: true location shift is greater than 0"
  }
  result <- list(statistic=W.ref,permutations=nperm,p.value=pvalue,alternative=alternative,H1=H1,method=method,data.name=data.name)
  class(result) <- c("list","perm.wilcox.test")
  return(result)
}
