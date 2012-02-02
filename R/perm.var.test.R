perm.var.test <-
function(formula,data=NULL,ratio=1,alternative=c("two.sided","less","greater"),nperm=999) {
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
  if (length(alternative)>1) {alternative <- "two.sided"}
  data.name <- paste("data: ",variables[1]," by ",variables[2],sep="")
  variance <- var(resp[fact==levels(fact)[1]],na.rm=TRUE)/var(resp[fact==levels(fact)[2]],na.rm=TRUE)
  names(variance) <- "ratio of variances"
  F.ref <- var.test(resp~fact,ratio=ratio,alternative=alternative)$statistic
  F.perm <- numeric(nperm+1)
  F.perm[1] <- F.ref
  for(i in 1:nperm) {
    F.perm[i+1] <- var.test(sample(resp)~fact,ratio=ratio,alternative=alternative)$statistic
  }
  pvalue <- NULL
  H1 <- NULL
  if (alternative=="two.sided") {
    pvalue <- min(length(which(F.perm <= F.ref)),length(which(F.perm >= F.ref)))*2/(nperm+1)
    H1 <- paste("alternative hypothesis: true ratio of variances is not equal to ",ratio,sep="")
  }
  if (alternative=="less") {
    pvalue <- length(which(F.perm <= F.ref))/(nperm+1)
    H1 <- paste("alternative hypothesis: true ratio of variances is less than ",ratio,sep="")
    }
  if (alternative=="greater") {
    pvalue <- length(which(F.perm >= F.ref))/(nperm+1)
    H1 <- paste("alternative hypothesis: true ratio of variances is greater than ",ratio,sep="")
  }
  result <- list(statistic=F.ref,permutations=nperm,p.value=pvalue,estimate=variance,alternative=alternative,H1=H1,
    data.name=data.name)
  class(result) <- c("list","perm.var.test")
  return(result)
}
