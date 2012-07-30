perm.bartlett.test <-
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
  K.ref <- bartlett.test(resp~fact)$statistic
  K.perm <- numeric(nperm+1)
  K.perm[1] <- K.ref
  pb <- txtProgressBar(min=0,max=100,initial=0,style=3)
  for(i in 1:nperm) {
    K.perm[i+1] <- bartlett.test(sample(resp)~fact)$statistic
    setTxtProgressBar(pb,round(i*100/nperm,0))
  }
  cat("\n")
  pvalue <- min(length(which(K.perm <= K.ref)),length(which(K.perm >= K.ref)))*2/(nperm+1)
  result <- list(statistic=K.ref,permutations=nperm,p.value=pvalue,data.name=data.name)
  class(result) <- c("list","perm.bartlett.test")
  return(result)
}
