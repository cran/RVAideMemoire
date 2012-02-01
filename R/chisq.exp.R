chisq.exp <-
function(data,p,graph=FALSE){
  if (length(p)!=nrow(data)){stop("number of expected proportions and populations differ")}
  n <- integer(nrow(data))
  for (i in 1:nrow(data)) {n[i] <- sum(data[i,])}
  n.theo1 <- n*p
  n.theo2 <- n*(1-p)
  n.theo.mat <- matrix(c(n.theo1,n.theo2),nrow=nrow(data),dimnames=list(rownames(data),colnames(data)))
  cochran.max <- ceiling(0.8*length(data))
  cochran.min <- length(data)-cochran.max
  result <- list(p.theo=p,mat=n.theo.mat,cochran=cochran.min)
  class(result) <- c("chisq.exp","list")
  if (graph==TRUE) {mosaicplot(t(n.theo.mat),main="Expected distribution",col=TRUE)}
  return(result)
}

