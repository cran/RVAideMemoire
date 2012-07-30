byf.shapiro <- function(x,f) {
  if (!is.factor(f)) {f <- factor(f)}
  data.name <- paste(deparse(substitute(x))," by ",deparse(substitute(f)),sep="")
  nlev <- nlevels(f)
  tab <- data.frame(W=integer(nlev),p.value=integer(nlev))
  rownames(tab) <- levels(f)
  for (i in 1:nlev) {
    test <- shapiro.test(x[as.numeric(f)==i])
    tab[i,1] <- test$statistic
    tab[i,2] <- test$p.value
  }
  result <- list(data.name=data.name,tab.result=tab)
  class(result) <- c("list","byf.shapiro")
  return(result)
}
