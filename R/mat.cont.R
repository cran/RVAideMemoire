mat.cont <- function(lev,ref=NULL,restrict=NULL,exclude=FALSE) {
  if (!is.null(ref)) {
    if (length(ref)>1) {stop("one reference level only")}
    if (!is.null(restrict) & !ref%in%restrict) {
	stop(paste(deparse(substitute(ref))," is not in the restricted levels",sep=""))
    }
  }
  lev2 <- if (is.null(restrict)) {
    lev
  } else {
    if (exclude) {lev <- lev[-which(!lev%in%restrict)]}
    lev[which(lev%in%restrict)]
  }
  comb <- t(combn(lev2,2))
  if (!is.null(ref)) {
    rows <- NULL
    for (i in 1:nrow(comb)) {
	if (ref%in%comb[i,]) {rows <- c(rows,i)}
    }
    comb <- comb[rows,]
  }
  result <- matrix(0,nrow=nrow(comb),ncol=length(lev),dimnames=list(1:nrow(comb),lev))
  for (i in 1:nrow(result)) {
    result[i,comb[i,1]] <- 1
    result[i,comb[i,2]] <- -1
  }
  for (i in 1:nrow(result)) {
    rownames(result)[i] <- paste(colnames(result)[which(result[i,]==1)]," - ",
	colnames(result)[which(result[i,]==-1)],sep="")
  }
  return(result)
}
