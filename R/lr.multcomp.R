lr.multcomp <-
function(formula,data,conf.level=0.95) {
  if (missing(formula)) {stop("formula missing")}
  if ((length(formula)!=3) || (length(formula[[3]])!=3) || (formula[[3]][[1]]!=as.name("|")) ||
    (length(formula[[3]][[2]])!=1) || (length(formula[[3]][[3]])!=1)) {stop("incorrect specification for formula")}
  formula[[3]][[1]] <- as.name("+")
  m <- match.call()
  m$formula <- formula
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  m$conf.level <- NULL
  mf <- eval(m,parent.frame())
  dname <- paste(names(mf)[1]," ~ ",names(mf)[2],", factor = ",names(mf)[3],sep="")
  y <- mf[,1]
  x <- mf[,2]
  fact <- mf[,3]
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
  result <- list(data.name=dname,conf.level=conf.level,n.reg=nlevels(fact2),intercepts=intcpt,slopes=slope,
    intercepts.comp=intcpt.comp,slopes.comp=slope.comp)
  class(result) <- "lr.multcomp"
  return(result)
}

print.lr.multcomp <-
function(x,...) {
  cat(paste("\nComparison of",x$n.reg,"least rectangles simple linear regressions\n\n"))
  cat(x$data.name,"\n\n")
  cat("Intercepts:\n")
  print(noquote(x$intercepts.comp),na.print="-")
  cat("\nSlopes:\n")
  print(noquote(x$slopes.comp),na.print="-")
  cat("\n")
}
