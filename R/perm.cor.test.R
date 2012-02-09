perm.cor.test <-
function(x,y,alternative=c("two.sided","less","greater"),nperm=999) {
  if (length(x)!=length(y)) {stop(paste("'",deparse(substitute(x)),"' and '",deparse(substitute(y)),"' lengths differ",sep=""))}
  if (!is.numeric(x)) {x <- as.numeric(as.character(x))}
  if (!is.numeric(y)) {y <- as.numeric(as.character(y))}
  if (length(alternative)>1) {alternative <- "two.sided"}
  data.name <- paste("data: ",deparse(substitute(x))," and ",deparse(substitute(y)),sep="")
  coeff <- cor.test(x,y,alternative=alternative)$estimate
  conf.int <- cor.test(x,y,alternative=alternative)$conf.int
  t.ref <- cor.test(x,y,alternative=alternative)$statistic
  t.perm <- numeric(nperm+1)
  t.perm[1] <- t.ref
  for(i in 1:nperm) {
    t.perm[i+1] <- cor.test(x,sample(y),alternative=alternative)$statistic
  }
  pvalue <- NULL
  H1 <- NULL
  if (alternative=="two.sided") {
    pvalue <- length(which(abs(t.perm) >= abs(t.ref)))/(nperm+1)
    H1 <- "alternative hypothesis: true correlation is not equal to 0"
  }
  if (alternative=="less") {
    pvalue <- length(which(t.perm <= t.ref))/(nperm+1)
    H1 <- "alternative hypothesis: true correlation is less than 0"
    }
  if (alternative=="greater") {
    pvalue <- length(which(t.perm >= t.ref))/(nperm+1)
    H1 <- "alternative hypothesis: true correlation is greater than 0"
  }
  result <- list(statistic=t.ref,permutations=nperm,p.value=pvalue,estimate=coeff,alternative=alternative,H1=H1,
    data.name=data.name,conf.int=conf.int)
  class(result) <- c("list","perm.cor.test")
  return(result)
}
