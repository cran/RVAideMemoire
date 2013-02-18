mat.cont <- function(f,ref=NULL,restrict=NULL,exclude=FALSE) {
  if (!is.null(ref)) {
    if (length(ref)>1) {stop("one reference level only")}
    if (!is.null(restrict) & !ref%in%restrict) {
	stop(paste(deparse(substitute(ref))," is not in the restricted levels",sep=""))
    }
  }
  if (!is.factor(f)) {f <- factor(f)}
  lev <- if (is.null(restrict)) {
    levels(f)
  } else {
    if (exclude) {f <- droplevels(subset(f,f%in%restrict))}
    levels(f)[which(levels(f)%in%restrict)]
  }
  comb <- t(combn(lev,2))
  if (!is.null(ref)) {
    rows <- NULL
    for (i in 1:nrow(comb)) {
	if (ref%in%comb[i,]) {rows <- c(rows,i)}
    }
    comb <- comb[rows,]
  }
  result <- matrix(0,nrow=nrow(comb),ncol=nlevels(f),dimnames=list(1:nrow(comb),levels(f)))
  for (i in 1:nrow(result)) {
    result[i,comb[i,1]] <- 1
    result[i,comb[i,2]] <- -1
  }
  colnames(result) <- abbreviate(colnames(result),3)
  for (i in 1:nrow(result)) {
    rownames(result)[i] <- paste(colnames(result)[which(result[i,]==1)],"-",
	colnames(result)[which(result[i,]==-1)],sep="")
  }
  return(result)
}
