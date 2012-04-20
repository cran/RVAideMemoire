wilcox.paired.multcomp <-
function(formula,data=NULL,p.method="fdr") {
  if (all.names(formula)[1]!="~" | all.names(formula)[3]!="|") {stop("incorrect 'formula'")}
  variables <- all.vars(formula)
  data.name <- paste(variables[1]," ~ ",variables[2],", block = ",variables[3],sep="")
  resp <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  fact <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  block <- if (is.null(data)) {get(variables[3],pos=environment(formula))}
    else {get(variables[3],pos=get(deparse(substitute(data))))}
  if (length(resp)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (length(resp)!=length(block)) {stop(paste("'",variables[1],"' and '",variables[3],"' lengths differ",sep=""))}
  if (length(fact)!=length(block)) {stop(paste("'",variables[2],"' and '",variables[3],"' lengths differ",sep=""))}
  if (!is.factor(fact)) {fact <- factor(fact)}
  if (!is.factor(block)) {block <- factor(block)}
  tab <- data.frame(fact,block,resp)
  tab <- tab[order(tab$fact),]
  method <- "Wilcoxon signed rank test"
  fun.p <- function(i,j) {
    test <- suppressWarnings(wilcox.test(tab$resp[as.integer(tab$fact)==i],tab$resp[as.integer(tab$fact)==j],paired=TRUE))
    test$p.value
  }
  comp <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
  result <- list(data.name=data.name,method=method,p.adjust.method=p.method,comp=comp)
  class(result) <- c("wilcox.paired.multcomp","list")
  return(result)
}

