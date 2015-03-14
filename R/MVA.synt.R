MVA.synt <-
function(x) {
  res <- list()
  if (inherits(x,"dudi")) {				# PCA/PCoA/COA/NSCA/MCA/Mix/Hillsmith: dudi.pca(), 
    if (inherits(x,"pca") | inherits(x,"pco") | # dudi.pco(), dudi.coa(), dudi.nsc, dudi.acm(), dudi.mix()
	inherits(x,"coa") | inherits(x,"acm") |	# and dudi.hillsmith() from ade4
	inherits(x,"mix") | inherits(x,"nsc")) {
	res$crit <- "total variance (%)"
	vars <- ade4::inertia.dudi(x)$TOT
	vars.each <- round(100*c(vars[1,"ratio"],diff(vars$ratio)),2)
	vars.tot <- round(100*vars$ratio,2)
	res$tab <- data.frame(Axis=1:nrow(vars),Proportion=vars.each,Cumulative=vars.tot)
	res$ncomp <- x$nf
    }
  } else if (inherits(x,"spca")) {			# sPCA: spca() from mixOmics
    res$crit <- "total variance (%)"
    vars <- c(x$varX[1],diff(x$varX))
    vars.each <- round(100*vars,2)
    vars.tot <- round(100*cumsum(vars),2)
    res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
    res$ncomp <- length(vars)
    res$keep <- x$keepX
  } else if (inherits(x,"prcomp")) {		# PCA: prcomp()
    res$crit <- "total variance (%)"
    vars <- summary(x)$importance
    vars.each <- round(100*vars["Proportion of Variance",],2)
    vars.tot <- round(100*vars["Cumulative Proportion",],2)
    res$tab <- data.frame(Axis=1:ncol(vars),Proportion=vars.each,Cumulative=vars.tot)
    res$ncomp <- ncol(vars)
  } else if (inherits(x,"princomp")) {		# PCA: princomp()
    res$crit <- "total variance (%)"
    vars <- summary(x)$sdev^2
    vars <- vars/sum(vars)
    vars.each <- round(100*vars,2)
    vars.tot <- round(100*cumsum(vars),2)
    res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
    res$ncomp <- length(vars)
  } else if (inherits(x,"pcoa")) {			# PCoA: pcoa() from ape
    res$crit <- "total variance (%)"
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
    res$tab <- data.frame(Axis=1:length(vars.each),Proportion=vars.each,Cumulative=vars.tot)
    res$ncomp <- as.numeric(readline("Number of components to be retained: "))
  } else if (inherits(x,"pco")) {			# PCoA: pco() from labdsv (wrapper to cmdscale())
    res$crit <- "total variance (%)"
    vars.each <- round(100*x$eig/sum(x$eig),2)
    vars.tot <- round(cumsum(vars.each),2)
    res$tab <- data.frame(Axis=1:length(vars.each),Proportion=vars.each,Cumulative=vars.tot)
    res$ncomp <- ncol(x$points)
  } else if (inherits(x,"nmds")) {			# NMDS: nmds() from labdsv (wrapper to isoMDS() from MASS)
    res$crit <- "stress"
    res$ncomp <- ncol(x$points)
    res$stress <- round(x$stress/100,3)
  } else if (inherits(x,"monoMDS")) {		# NMDS: monoMDS() from vegan
    res$crit <- "stress"
    res$ncomp <- x$ndim
    res$stress <- round(x$stress,3)
  } else if (inherits(x,"metaMDS")) {		# NMDS: metaMDS() from vegan
    res$crit <- "stress"
    if (inherits(x,"monoMDS")) {
	res$ncomp <- x$ndim
	res$stress <- round(x$stress,3)
    } else {
	res$ncomp <- x$ndim
	res$stress <- round(x$stress/100,3)
    }
  } else if (inherits(x,"lda")) {			# LDA: lda() from MASS
    model <- LDA.format(x)
    Y <- model$grouping
    if (nlevels(Y) == 2) {stop("only 2 levels, hence 1 axis -> 100% of variance explained by this axis")}
    res$crit <- "intergroup variance (%)"
    res$ncomp <- ncol(model$li)
    ncomp.tot <- res$ncomp
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
    res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  } else if (inherits(x,"discrimin")) {		# LDA/CDA: discrimin() from ade4
    appel <- as.list(x$call)
    Y <- eval.parent(appel$fac)
    if (nlevels(Y) == 2) {stop("only 2 levels, hence 1 axis -> 100% of variance explained by this axis")}
    res$crit <- "intergroup variance (%)"
    res$ncomp <- x$nf
    call2 <- x$call
    call2$nf <- nlevels(Y)-1
    call2$scannf <- FALSE
    x2 <- eval.parent(call2)
    ncomp.tot <- call2$nf
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
    res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  } else if (inherits(x,"plsda")) {			# PLS-DA: plsda() from mixOmics
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
    res$crit <- "intergroup variance (%)"
    res$ncomp <- ncomp <- x$ncomp
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
    res$tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.tot)
  }
  class(res) <- "MVA.synt"
  return(res)
}

print.MVA.synt <- function(x,...) {
  cat(paste0("Criterion: ",x$crit,"\n"))
  if ("keep" %in% names(x)) {
    cat(paste0("Number of variables retained: ",paste(x$keep,collapse=","),"\n"))
  }
  if ("tab" %in% names(x)) {
    cat("\n")
    print(x$tab[1:x$ncomp,],row.names=FALSE)
  }
  if ("stress" %in% names(x)) {
    cat(paste0("Axes: ",x$ncomp,"\n"))
    cat(paste0("Stress: ",x$stress,"\n"))
  }
}


