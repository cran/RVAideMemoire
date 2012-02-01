perm.kruskal.test <-
function(formula,data=NULL,nperm=999) {
  if (all.names(formula)[1]!="~") {stop("incorrect 'formula'")}
  variables<-all.vars(formula)
  resp <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  fact <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  if (length(resp)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (!is.numeric(resp)) {resp <- as.numeric(as.character(resp))}
  if (!is.factor(fact)) {fact <- factor(fact)}
  data.name <- paste("data: ",variables[1]," by ",variables[2],sep="")
  chi.ref <- kruskal.test(resp~fact)$statistic
  chi.perm <- numeric(nperm+1)
  chi.perm[1] <- chi.ref
  for(i in 1:nperm) {
    chi.perm[i+1] <- kruskal.test(sample(resp)~fact)$statistic
  }
  pvalue <- min(length(which(chi.perm <= chi.ref)),length(which(chi.perm >= chi.ref)))*2/(nperm+1)
  result <- list(statistic=chi.ref,permutations=nperm,p.value=pvalue,data.name=data.name)
  class(result) <- c("list","perm.kruskal.test")
  return(result)
}
