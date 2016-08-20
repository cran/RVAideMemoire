cov.test <- function(X,Y,scale.X=TRUE,scale.Y=TRUE,nperm=999,progress=TRUE) {
  dname <- paste0(deparse(substitute(X))," and ",deparse(substitute(Y)),
    "\n",nperm," permutations")
  if (!is.data.frame(X)) {X <- as.data.frame(X)}
  if (!is.data.frame(Y)) {Y <- as.data.frame(Y)}
  if (scale.X) {X <- scale(X)}
  if (scale.Y) {Y <- scale(Y)}
  ref <- sum(cov(X,Y)^2)
  names(ref) <- "Square covariance"
  stat.perm <- numeric(nperm+1)
  stat.perm[1] <- ref
  if (progress) {pb <- txtProgressBar(min=0,max=100,initial=0,style=3)}
  n <- nrow(Y)
  rownames(X) <- rownames(Y) <- 1:n
  for (i in 1:nperm) {
    if (progress) {setTxtProgressBar(pb,round(i*100/nperm,0))}
    Y.perm <- Y[sample(1:n),]
    stat.perm[i] <- sum(cov(X,Y.perm)^2)
  }
  if (progress) {cat("\n")}
  pvalue <- length(which((stat.perm+.Machine$double.eps/2) >= ref))/(nperm+1)
  result <- list(method="Permutation test based on square covariance",data.name=dname,
    statistic=ref,permutations=nperm,p.value=pvalue)
  class(result) <- "htest"
  return(result)
}
