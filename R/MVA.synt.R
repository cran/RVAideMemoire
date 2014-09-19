MVA.synt <- function(x) {
  UseMethod("MVA.synt")
}

MVA.synt.dudi <- function(x) {
  UseMethod("MVA.synt.dudi")
}

print.MVA.synt <- function(x,...) {
  cat(paste0("Criterion: ",x$crit,"\n"))
  if ("keep" %in% names(x)) {
    cat(paste0("Number of variables retained: ",paste(x$keep,collapse=","),"\n"))
  }
  if ("tab" %in% names(x)) {
    cat("\n")
    print(x$tab,row.names=FALSE)
  }
  if ("stress" %in% names(x)) {
    cat(paste0("Axes: ",x$ncomp,"\n"))
    cat(paste0("Stress: ",x$stress,"\n"))
  }
}


### PCA

# dudi.pca() from ade4:
MVA.synt.dudi.pca <- function(x) {
  vars <- ade4::inertia.dudi(x)$TOT
  res <- list(crit="total variance (%)")
  vars.each <- round(100*c(vars[1,"ratio"],diff(vars$ratio)),2)
  vars.tot <- round(100*vars$ratio,2)
  tab <- data.frame(Axis=1:nrow(vars),Proportion=vars.each,Cumulative=vars.tot)
  res$tab <- tab[1:x$nf,]
  class(res) <- "MVA.synt"
  return(res)
}

# prcomp(), works also with pca() from mixOmics:
MVA.synt.prcomp <- function(x) {
  vars <- summary(x)$importance
  res <- list(crit="total variance (%)")
  vars.each <- round(100*vars["Proportion of Variance",],2)
  vars.tot <- round(100*vars["Cumulative Proportion",],2)
  res$tab <- data.frame(Axis=1:ncol(vars),Proportion=vars.each,Cumulative=vars.tot)
  class(res) <- "MVA.synt"
  return(res)
}

# princomp():
MVA.synt.princomp <- function(x) {
  vars <- summary(x)$sdev^2
  vars <- vars/sum(vars)
  res <- list(crit="total variance (%)")
  vars.each <- round(100*vars,2)
  vars.tot <- round(100*cumsum(vars),2)
  res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  class(res) <- "MVA.synt"
  return(res)
}


### sPCA

# spca() from mixOmics:
MVA.synt.spca <- function(x) {
  vars <- c(x$varX[1],diff(x$varX))
  res <- list(crit="total variance (%)",keep=x$keepX)
  vars.each <- round(100*vars,2)
  vars.tot <- round(100*cumsum(vars),2)
  res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  class(res) <- "MVA.synt"
  return(res)
}


### IPCA

# ipca() from mixOmics:
# MVA.synt.ipca <- function(x) {
#   ncomp <- x$ncomp
#   ncomp.tot <- nrow(x$loadings)
#   call2 <- x$call
#   call2$ncomp <- ncomp.tot
#   x2 <- eval.parent(call2)
#   coord <- x2$x
#   vars <- matrix(0,ncol=ncomp.tot,nrow=1,dimnames=list(1,paste("comp",1:ncomp.tot,sep="")))
#   for (i in 1:ncomp.tot) {
#     vars[1,i] <- sum(coord[,i]^2)/(nrow(coord))
#   }
#   vars.each <- round(as.vector(100*vars/sum(vars)),2)
#   vars.tot <- round(cumsum(vars.each),2)
#   res <- list(crit="total variance (%)")
#   tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
#   res$tab <- tab[1:ncomp,]
#   class(res) <- "MVA.synt"
#   return(res)
# }


### sIPCA

# sipca() from mixOmics:
# MVA.synt.sipca <- function(x) {
#
# }


### PCoA

# pcoa() from ape:
MVA.synt.pcoa <- function(x) {
  ncomp <- as.numeric(readline("Number of components to be retained: "))
  res <- list(crit="total variance (%)")
  vars.each <- if ("Rel_corr_eig" %in% colnames(x$values)) {
    round(100*x$values$Rel_corr_eig,2)
  } else {
    round(100*x$values$Relative_eig,2)
  }
  vars.tot <- if ("Cum_corr_eig" %in% colnames(x$values)) {
    round(100*x$values$Cum_corr_eig,2)
  } else {
    round(100*x$values$Cumul_eig,2)
  }
  tab <- data.frame(Axis=1:length(vars.each),Proportion=vars.each,Cumulative=vars.tot)
  res$tab <- tab[1:ncomp,]
  class(res) <- "MVA.synt"
  return(res)
}

# dudi.pco() from ade4:
MVA.synt.dudi.pco <- MVA.synt.dudi.pca

# pco() from labdsv (wrapper to cmdscale()):
MVA.synt.pco <- function(x) {
  res <- list(crit="total variance (%)")
  ncomp <- ncol(x$points)
  vars.each <- round(100*x$eig/sum(x$eig),2)
  vars.tot <- round(cumsum(vars.each),2)
  tab <- data.frame(Axis=1:length(vars.each),Proportion=vars.each,Cumulative=vars.tot)
  res$tab <- tab[1:ncomp,]
  class(res) <- "MVA.synt"
  return(res)
}


### NMDS

# nmds() from labdsv (wrapper to isoMDS() from MASS):
MVA.synt.nmds <- function(x) {
  res <- list(crit="stress")
  res$ncomp <- ncol(x$points)
  res$stress <- round(x$stress/100,3)
  class(res) <- "MVA.synt"
  return(res)
}

# monoMDS() from vegan:
MVA.synt.monoMDS <- function(x) {
  res <- list(crit="stress")
  res$ncomp <- x$ndim
  res$stress <- round(x$stress,3)
  class(res) <- "MVA.synt"
  return(res)
}

# metaMDS() from vegan:
MVA.synt.metaMDS <- function(x) {
  UseMethod("MVA.synt.metaMDS")
}

MVA.synt.metaMDS.default <- function(x) {
  res <- list(crit="stress")
  res$ncomp <- x$ndim
  res$stress <- round(x$stress/100,3)
  class(res) <- "MVA.synt"
  return(res)
}

MVA.synt.metaMDS.monoMDS <- MVA.synt.monoMDS


### COA

# dudi.coa() from ade4:
MVA.synt.dudi.coa <- MVA.synt.dudi.pca


### LDA

# lda() from MASS:
MVA.synt.lda <- function(x) {
  model <- LDA.format(x)
  Y <- model$grouping
  if (nlevels(Y) == 2) {stop("only 2 levels, hence 1 axis -> 100% of variance explained by this axis")}
  ncomp <- ncol(model$li)
  ncomp.tot <- ncomp
  coord <- model$li
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
  vars.each <- round(as.vector(100*vars/sum(vars)),2)
  vars.tot <- round(cumsum(vars.each),2)
  res <- list(crit="intergroup variance (%)")
  res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  class(res) <- "MVA.synt"
  return(res)
}


## LDA/CDA

# discrimin() from ade4:
MVA.synt.discrimin <- function(x) {
  appel <- as.list(x$call)
  Y <- eval.parent(appel$fac)
  if (nlevels(Y) == 2) {stop("only 2 levels, hence 1 axis -> 100% of variance explained by this axis")}
  call2 <- x$call
  call2$nf <- nlevels(Y)-1
  x2 <- eval.parent(call2)
  ncomp.tot <- call2$nf
  ncomp <- x$nf
  coord <- x2$li
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
  vars.each <- round(as.vector(100*vars/sum(vars)),2)
  vars.tot <- round(cumsum(vars.each),2)
  res <- list(crit="intergroup variance (%)")
  res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  class(res) <- "MVA.synt"
  return(res)
}


## PLS-DA

# plsda() from mixOmics:
MVA.synt.plsda <- function(x) {
  if (packageVersion("mixOmics")<"5.0.2") {
    stop(paste("you must update 'mixOmics' to version >= 5.0.2 (actual: ",
	packageVersion("mixOmics"),")",sep=""))
  }
  X <- x$X
  Y <- character(nrow(X))
  for (i in 1:length(Y)) {
    if (any(x$ind.mat[i,]!=0)) {
	Y[i] <- x$names$Y[which(x$ind.mat[i,]==1)]
    } else {
	Y[i] <- NA
    }
  }
  Y <- factor(Y)
  if (any(is.na(Y))) {
    X <- X[-which(is.na(Y)),]
    Y <- na.omit(Y)
  }
  ncomp <- x$ncomp
  ncomp.tot <- ncol(X)
  model2 <- mixOmics::plsda(X,Y,ncomp=ncomp.tot)
  coord <- model2$variates$X
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
  vars.each <- round(as.vector(100*vars/sum(vars)),2)
  vars.tot <- round(cumsum(vars.each),2)
  res <- list(crit="intergroup variance (%)")
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  res$tab <- tab[1:ncomp,]
  class(res) <- "MVA.synt"
  return(res)
}
