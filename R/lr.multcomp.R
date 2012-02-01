lr.multcomp <-
function(formula,data=NULL,conf.level=0.95) {
  if (all.names(formula)[1]!="~" | all.names(formula)[3]!="|") {stop("incorrect 'formula'")}
  variables <- all.vars(formula)
  y <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
    else {get(variables[1],pos=get(deparse(substitute(data))))}
  x <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
    else {get(variables[2],pos=get(deparse(substitute(data))))}
  fact <- if (is.null(data)) {get(variables[3],pos=environment(formula))}
    else {get(variables[3],pos=get(deparse(substitute(data))))}
  data.name <- paste(variables[1]," ~ ",variables[2],", factor = ",variables[3],sep="")
  if (length(x)!=length(y)) {stop(paste("'",variables[2],"' and '",variables[1],"' lengths differ",sep=""))}
  if (length(x)!=length(fact)) {stop(paste("'",variables[2],"' and '",variables[3],"' lengths differ",sep=""))}
  if (length(y)!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[3],"' lengths differ",sep=""))}
  if (is.character(fact) & !is.factor(fact)) {fact <- factor(fact)}
  nul <- as.numeric(row.names(table(c(which(is.na(y)),which(is.na(x))))))
  y2 <- if(length(nul)>0) {y[-nul]} else {y}
  x2 <- if(length(nul)>0) {x[-nul]} else {x}
  fact2 <- if(length(nul)>0) {fact[-nul]} else {fact}
  intcpt <- matrix(ncol=3,nrow=nlevels(fact2),dimnames=list(levels(fact2),c("Inf","Intcpt","Sup")))
  slope <- matrix(ncol=3,nrow=nlevels(fact2),dimnames=list(levels(fact2),c("Inf","Slope","Sup")))
  for (i in 1:nlevels(fact2)) {
    x.temp <- x2[fact2==levels(fact2)[i]]
    y.temp <- y2[fact2==levels(fact2)[i]]
    model <- least.rect(y.temp~x.temp,conf.level=conf.level)
    intcpt[i,] <- model$conf.int[1,]
    slope[i,] <- model$conf.int[2,]
  }
  intcpt.comp <- matrix(nrow=nlevels(fact2),ncol=nlevels(fact2),dimnames=list(levels(fact2),levels(fact2)))
  slope.comp <- matrix(nrow=nlevels(fact2),ncol=nlevels(fact2),dimnames=list(levels(fact2),levels(fact2)))
  comb <- combn(levels(fact2),2)
  for (i in 1:ncol(comb)) {
    intcpt.comp[comb[2,i],comb[1,i]] <- if (intcpt[comb[1,i],2]>intcpt[comb[2,i],2]) {
	if (intcpt[comb[1,i],1]>intcpt[comb[2,i],3]) {"unequal"} else {"equal"}
    } else if (intcpt[comb[1,i],2]<intcpt[comb[2,i],2]) {
	if (intcpt[comb[1,i],3]<intcpt[comb[2,i],1]) {"unequal"} else {"equal"}
    } else {"equal"}
    slope.comp[comb[2,i],comb[1,i]] <- if (slope[comb[1,i],2]>slope[comb[2,i],2]) {
	if (slope[comb[1,i],1]>slope[comb[2,i],3]) {"unequal"} else {"equal"}
    } else if (slope[comb[1,i],2]<slope[comb[2,i],2]) {
	if (slope[comb[1,i],3]<slope[comb[2,i],1]) {"unequal"} else {"equal"}
    } else {"equal"}
  }
  slope.comp <- slope.comp[-1,-ncol(slope.comp)]
  intcpt.comp <- intcpt.comp[-1,-ncol(intcpt.comp)]
  result <- list(data.name=data.name,conf.level=conf.level,n.reg=nlevels(fact2),intercepts=intcpt,slopes=slope,
    intercepts.comp=intcpt.comp,slopes.comp=slope.comp)
  class(result) <- c("lr.multcomp","list")
  return(result)
}

