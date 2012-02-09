perm.anova <-
function(formula,nest.f2=c("fixed","random"),data=NULL,nperm=999) {
  if (all.names(formula)[1]!="~") {stop("incorrect 'formula'")}
  variables<-all.vars(formula)
  resp <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  fact1 <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  if (length(resp)!=length(fact1)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (!is.numeric(resp)) {resp <- as.numeric(as.character(resp))}
  if (!is.factor(fact1)) {fact1 <- factor(fact1)}
  if (length(nest.f2)>1) {nest.f2 <- "fixed"}
  data.name <- paste("Response: ",variables[1],sep="")
  tab <- NULL
  if (length(variables)==2) {
    tab <- perm.anova.1way(resp,fact1,variables,nperm)$tab
  } else if (length(variables)==3) {
    fact2 <- if (is.null(data)) {get(variables[3],pos=environment(formula))}
	else {get(variables[3],pos=get(deparse(substitute(data))))}
    if (length(resp)!=length(fact2)) {stop(paste("'",variables[1],"' and '",variables[3],"' lengths differ",sep=""))}
    if (!is.factor(fact2)) {fact2 <- factor(fact2)}
    if (all.names(formula)[3]=="+") {
	tab <- perm.anova.2wayA(resp,fact1,fact2,variables,nperm)$tab
    } else if (all.names(formula)[3]=="*") {
	tab <- perm.anova.2wayB(resp,fact1,fact2,variables,nperm)$tab
    } else if (all.names(formula)[3]=="/") {
	tab <- perm.anova.2wayC(resp,fact1,fact2,nest.f2,variables,nperm)$tab
    } else if (all.names(formula)[3]=="|") {
	data.name <- paste(data.name,"\nBlock: ",variables[3],sep="")
	tab <- perm.anova.2wayD(resp,fact1,fact2,variables,nperm)$tab
    }
  } else if (length(variables)==4) {
    if (all.names(formula)[3]!="|") {stop("incorrect 'formula'")}
    fact2 <- if (is.null(data)) {get(variables[3],pos=environment(formula))}
	else {get(variables[3],pos=get(deparse(substitute(data))))}
    fact3 <- if (is.null(data)) {get(variables[4],pos=environment(formula))}
	else {get(variables[4],pos=get(deparse(substitute(data))))}
    if (length(resp)!=length(fact2)) {stop(paste("'",variables[1],"' and '",variables[3],"' lengths differ",sep=""))}
    if (length(resp)!=length(fact3)) {stop(paste("'",variables[1],"' and '",variables[4],"' lengths differ",sep=""))}
    if (!is.factor(fact2)) {fact2 <- factor(fact2)}
    if (!is.factor(fact3)) {fact2 <- factor(fact3)}
    data.name <- paste(data.name,"\nBlock: ",variables[4],sep="")
    if (all.names(formula)[4]=="+") {
	tab <- perm.anova.3wayA(resp,fact1,fact2,fact3,variables,nperm)$tab
    } else if (all.names(formula)[4]=="*") {
	tab <- perm.anova.3wayB(resp,fact1,fact2,fact3,variables,nperm)$tab
    } else {
	stop("only additive and multiplicative models are permitted")
    }
  }
  tab[1:(nrow(tab)-1),ncol(tab)] <- psignif(tab[1:(nrow(tab)-1),"Pr(>F)"])
  result <- list(permutations=nperm,data.name=data.name,table=tab)
  class(result) <- c("list","perm.anova")
  return(result)
}
