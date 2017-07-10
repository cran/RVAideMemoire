# mixOmics: perf

DIABLO.test <- function(x,method=c("mahalanobis.dist","max.dist","centroids.dist"),validation=c("Mfold","loo"),k=7,
  nperm=999,progress=TRUE,...) {
  dname <- paste0(deparse(substitute(x)),"\nDIABLO (",x$ncomp[1]," components)\n",nperm," permutations")
  p.ref <- suppressWarnings(DIABLO.cv(x,method=method,validation=validation,k=k,repet=20,...))
  ref <- c("CER"=p.ref$NMC.mean)
  stat.perm <- numeric(nperm+1)
  stat.perm[1] <- ref
  if (progress) {pb <- txtProgressBar(min=0,max=100,initial=0,style=3)}
  for (i in 1:nperm) {
    if (progress) {setTxtProgressBar(pb,round(i*100/nperm,0))}
    x$Y <- sample(x$Y)
    p.perm <- suppressWarnings(DIABLO.cv(x,method=method,validation=validation,k=k,repet=1,...))
    stat.perm[i+1] <- p.perm$NMC.mean
  }
  if (progress) {cat("\n")}
  pvalue <- length(which((stat.perm-.Machine$double.eps/2) <= ref))/(nperm+1)
  result <- list(method="permutation test based on cross-validation",data.name=dname,statistic=ref,permutations=nperm,
    p.value=pvalue)
  class(result) <- "htest"
  return(result)
}


