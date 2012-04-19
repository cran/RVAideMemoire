cramer.coeff <-
function (var1,var2,nrep=1000,conf.level=0.95) {
  if (length(var1)!=length(var2)) {
    stop(paste("'",deparse(substitute(var1)),"' and '",deparse(substitute(var2)),"' lengths differ",sep=""))
  }
  data.name <- paste("data: ",deparse(substitute(var1))," and ",deparse(substitute(var2)),sep="")
  if (!is.factor(var1)) {var1 <- factor(var1)}
  if (!is.factor(var2)) {var2 <- factor(var2)}
  nul <- as.numeric(row.names(table(c(which(is.na(var1)), which(is.na(var2))))))
  var1.2 <- if (length(nul)>0) {
    var1[-nul]
  } else {
    var1
  }
  var2.2 <- if (length(nul)>0) {
    var2[-nul]
  } else {
    var2
  }
  if (any(tapply(var1.2,var1.2,function(x) length(x)/length(var1.2))<0.05) |
    any(tapply(var2.2,var2.2,function(x) length(x)/length(var2.2))<0.05)) {
    warning("at least 1 level contains less than 5% of total number of individuals")
  }
  tab.cont <- table(var1.2,var2.2)
  v <- sqrt(as.numeric(suppressWarnings(chisq.test(tab.cont)$statistic))/(sum(tab.cont)*(min(dim(tab.cont))-1)))
  names(v) <- "V"
  v.fun <- function(dat,ind) {
    cont <- table(dat[ind,1],dat[ind,2])
    sqrt(as.numeric(suppressWarnings(chisq.test(cont)$statistic))/(sum(cont)*(min(dim(cont))-1)))
  }
  simul <- boot(data.frame(var1.2, var2.2),v.fun,R=nrep)
  interval <- ci(simul$t,conf.level=conf.level)
  test <- suppressWarnings(chisq.test(tab.cont))
  result <- list(statistic=test$statistic,parameter=test$parameter,p.value=test$p.value,data.name=data.name,
    estimate=v,conf.level=conf.level,rep=nrep,interval=interval)
  class(result) <- c("cramer.coeff","list")
  return(result)
}
