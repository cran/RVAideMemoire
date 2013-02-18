DA.var <- function(model) {
  if (class(model)!="lda") {stop("model not recognized")}
  if (class(model)=="lda") {
    model <- LDA.format(model)
    Y <- model$grouping
    if (nlevels(Y)==2) {stop("only 2 levels, hence 1 axis -> 100% of variance explained by this axis")}
    ncomp <- ncol(model$li)
    ncomp.tot <- ncomp
    coord <- model$li
  }
  weights <- table(Y)
  means <- aggregate(coord,list(Y=Y),mean)[,-1]
  col.means <- matrix(0,ncol=ncomp.tot,nrow=1,dimnames=list(1,paste("comp",1:ncomp.tot,sep="")))
  for (i in 1:ncomp.tot) {
    col.means[1,i] <- sum(means[,i]*weights)/sum(weights)
  }
  vars <- matrix(0,ncol=ncomp.tot,nrow=1,dimnames=list(1,paste("comp",1:ncomp.tot,sep="")))
  for (i in 1:ncomp.tot) {
    vars[1,i] <- sum((means[,i]-col.means[1,i])^2*weights)/sum(weights)
  }
  prop <- round(as.vector(100*vars/sum(vars)),2)
  tab <- data.frame("Proportion (%)"=prop,Cumulative=round(cumsum(prop),2),
    row.names=paste("Comp",1:ncomp.tot,sep=""),check.names=FALSE)
  return(tab)
}
