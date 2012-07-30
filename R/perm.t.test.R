perm.t.test <-
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
  moy <- NULL
  t.ref <- t.test(resp~fact,var.equal=TRUE,alternative=alternative,paired=paired)$statistic
  t.perm <- numeric(nperm+1)
  t.perm[1] <- t.ref
  pb <- txtProgressBar(min=0,max=100,initial=0,style=3)
  if (!paired) {
    method <- "     Permutational Two Sample t-test"
    moy <- tapply(resp,fact,mean)
    names(moy) <- paste("mean in group ",levels(fact),sep="")
    for(i in 1:nperm) {
	t.perm[i+1] <- t.test(sample(resp)~fact,var.equal=TRUE,alternative=alternative,paired=FALSE)$statistic
	setTxtProgressBar(pb,round(i*100/nperm,0))
    }
  } else {
    method <- "     Permutational Paired t-test"
    moy <- mean(resp[fact==levels(fact)[1]]-resp[fact==levels(fact)[2]])
    names(moy) <- "mean of the differences"
    resp2 <- cbind(resp[fact==levels(fact)[1]],resp[fact==levels(fact)[2]])
    for (i in 1:nperm) {
	resp.perm <- t(apply(resp2,1,sample))
	t.perm[i+1] <- t.test(resp.perm[,1],resp.perm[,2],alternative=alternative,paired=TRUE)$statistic
	setTxtProgressBar(pb,round(i*100/nperm,0))
    }
  }
  cat("\n")
  pvalue <- NULL
  H1 <- NULL
  if (alternative=="two.sided") {
    pvalue <- length(which(abs(t.perm) >= abs(t.ref)))/(nperm+1)
    H1 <- "alternative hypothesis: true difference in means is not equal to 0"
  }
  if (alternative=="less") {
    pvalue <- length(which(t.perm <= t.ref))/(nperm+1)
    H1 <- "alternative hypothesis: true difference in means is less than 0"
    }
  if (alternative=="greater") {
    pvalue <- length(which(t.perm >= t.ref))/(nperm+1)
    H1 <- "alternative hypothesis: true difference in means is greater than 0"
  }
  result <- list(statistic=t.ref,permutations=nperm,p.value=pvalue,estimate=moy,alternative=alternative,H1=H1,
    method=method,data.name=data.name)
  class(result) <- c("list","perm.t.test")
  return(result)
}
