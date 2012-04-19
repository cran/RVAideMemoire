spearman.ci <-
function(var1,var2,nrep=1000,conf.level=0.95){
  if (length(var1)!=length(var2)) {stop(paste("'",deparse(substitute(var1)),"' and '",
    deparse(substitute(var2)),"' lengths differ",sep=""))}
  nul <- as.numeric(row.names(table(c(which(is.na(var1)),which(is.na(var2))))))
  var1.2 <- if(length(nul)>0) {var1[-nul]} else {var1}
  var2.2 <- if(length(nul)>0) {var2[-nul]} else {var2}
  cor.fun <- function(data,ind) {
    as.numeric(suppressWarnings(cor.test(data[ind,1],data[ind,2],method="spearman")$estimate))
  }
  simul <- boot(data.frame(var1.2,var2.2),cor.fun,R=nrep)
  interval <- ci(simul$t,conf.level=conf.level)
  result <- list(conf.level=conf.level,rep=nrep,
    coeff=as.numeric(suppressWarnings(cor.test(var1,var2,method="spearman")$estimate)),interval=interval)
  class(result) <- c("spearman.ci","list")
  return(result)
}

