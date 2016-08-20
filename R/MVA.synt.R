# ade4: dudi.coa
# pls: R2

#  - Total variance:
#     * PCA (dudi.pca[ade4],prcomp[stats],princomp[stats],pca[mixOmics],pca[labdsv],rda[vegan])
#     * sPCA (spca[mixOmics])
#     * PCoA (dudi.pco[ade4],pcoa[ape],pco[labdsv],cmdscale[stats]°,wcmdscale[vegan]°,capscale[vegan])
#		° if computed with 'eig=TRUE'
#	  Does not take into account imaginary axes
#     * RDA (pcaiv[ade4],pcaivortho[ade4],rda[vegan])
#     * db-RDA (capscale[vegan],dbrda[vegan])
#     * GPA (GPA[FactoMineR])
#     * RGCCA (wrapper.rgcca[mixOmics],rgcca[RGCCA])
#
#  - Consensus variance:
#     * GPA (GPA[FactoMineR])
#
#  - Residual variance:
#     * GPA (GPA[FactoMineR])
#
#  - Total inertia:
#     * COA (dudi.coa[ade4],cca[vegan])
#     * CCA (cca[ade4],cca[vegan])
#     * MCA (dudi.acm[ade4])
#     * Mix analysis (dudi.mix[ade4],dudi.hillsmith[ade4])
#
#  - Constrained variance:
#     * RDA (pcaiv[ade4],rda[vegan])
#     * db-RDA (capscale[vegan],dbrda[vegan])
#
#  - Unconstrained variance:
#     * RDA (pcaivortho[ade4],rda[vegan])
#     * db-RDA (capscale[vegan],dbrda[vegan])
#
#  - Constrained inertia:
#     * CCA (cca[ade4],cca[vegan])
#
#  - Unconstrained inertia:
#     * CCA (cca[vegan])
#
#  - Co-inertia:
#     * CIA (coinertia[ade4])
#
#  - X variance + Y variance:
#     * CPPLS (mvr[pls])
#     * PLSR (mvr[pls],plsR[plsRglm] !que Y!, !!! pls[mixOmics])
#     * PCR (mvr[pls])
#
#  - Square covariance:
#     * 2B-PLS (pls[mixOmics])
#
#  - Correlation between pairs of axes:
#     * 2B-PLS (pls[mixOmics])
#     * CCorA (CCorA[vegan],rcc[mixOmics])
#     * rCCorA (rcc[mixOmics])
#     * CIA (coinertia[ade4])
#     * RGCCA (wrapper.rgcca[mixOmics],rgcca[RGCCA])
#
#  - Kurtosis:
#     * IPCA (ipca[mixOmics])
#     * sIPCA (sipca[mixOmics])
#
#  - Stress:
#     * nMDS (isoMDS[MASS],monoMDS[vegan],metaMDS[vegan],nmds[labdsv])
#
#  - RV coefficient:
#     * CIA (coinertia[ade4])
#
#  - m2:
#     * PCIA (procuste[ade4])


print.MVA.synt <- function(x,...) {
  if ("cors" %in% names(x)) {
    to.print <- min(length(x$cors),x$rows)
    for (i in 1:to.print) {
	x.i <- x$cors[[i]]
	cat(paste0("Criterion: ",x.i$crit,"\n"))
	print(round(x.i$tab,4))
	cat("\n")
    }
  }
  if ("RV" %in% names(x)) {
    cat(paste0("RV coefficient: ",round(x$RV,4),"\n\n"))
  }
  for (i in 1:(length(x)-1)) {
    if (i==1) {if (names(x)[[1]] %in% c("cors","RV")) {next}}
    x.i <- x[[i]]
    cat(paste0("Criterion: ",x.i$crit,"\n"))
    if ("tab" %in% names(x.i)) {
	if (x$rows>nrow(x.i$tab)) {
	  rows <- nrow(x.i$tab)
	} else {
	  rows <- x$rows
	}
	tab <- x.i$tab
	if ("Kurtosis" %in% colnames(tab)) {
	  tab[,"Kurtosis"] <- round(tab[,"Kurtosis"],2)
	  print(tab[1:rows,],row.names=FALSE)
	} else	if ("Proportion" %in% colnames(tab)) {
	  tab[,"Proportion"] <- round(tab[,"Proportion"],2)
	  if ("Cumulative" %in% colnames(tab)) {
	    tab[,"Cumulative"] <- round(tab[,"Cumulative"],2)
	    print(tab[1:rows,],row.names=FALSE)
	  } else {
	    print(tab,row.names=FALSE)
	  }
	} else if ("Correlation" %in% colnames(tab)) {
	  tab[,"Correlation"] <- round(tab[,"Correlation"],4)
	  print(tab[1:rows,],row.names=FALSE)
	}
    }
    if ("m2" %in% names(x.i)) {
	cat(paste0("m2: ",round(x.i$m2,4),"\n"))
    }
    if ("stress" %in% names(x.i)) {
	cat(paste0("Stress: ",round(x.i$stress,4),"\n"))
    }
    cat("\n")
  }
}

MVA.synt <- function(x,rows=5) {
  x <- MVA.ident(x)
  res <- MVA.get.synt(x)
  res$rows <- rows
  class(res) <- "MVA.synt"
  return(res)
}

MVA.get.synt <- function(x,...) {
  UseMethod("MVA.get.synt")
}

MVA.get.synt.default <- function(x,...) {
  stop("unknown multivariate analysis")
}

MVA.get.synt.PCA.ade4 <- MVA.get.synt.PCoA.ade4 <- function(x,...) {
  res <- list()
  vars <- x$eig
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCA.prcomp <- function(x,...) {
  res <- list()
  vars <- summary(x)$importance
  vars.each <- 100*vars["Proportion of Variance",]
  vars.cum <- 100*vars["Cumulative Proportion",]
  tab <- data.frame(Axis=1:ncol(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCA.princomp <- function(x,...) {
  res <- list()
  vars <- summary(x)$sdev^2
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCA.mixOmics <- function(x,...) {
  res <- list()
  comp <- x$ncomp
  comp.max <- min(ncol(x$X),nrow(x$X)-1)
  x <- update(x,ncomp=comp.max)
  vars <- x$sdev
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  tab <- tab[1:comp,]
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCA.labdsv <- function(x,...) {
  res <- list()
  vars <- x$sdev^2
  vars <- vars/x$totdev
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCA.vegan <- function(x,...) {
  res <- list()
  vars <- x$CA$eig
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.sPCA.mixOmics <- function(x,...) {
  res <- list()
  vars.each <- 100*c(x$varX[1],diff(x$varX))
  vars.cum <- 100*x$varX
  tab <- data.frame(Axis=1:length(vars.each),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.IPCA.mixOmics <- MVA.get.synt.sIPCA.mixOmics <- function(x,...) {
  res <- list()
  kurt <- x$kurtosis
  tab <- data.frame(Axis=1:length(kurt),Kurtosis=kurt)
  res[[1]] <- list(crit="kurtosis",tab=tab)
  return(res)
}

MVA.get.synt.PCoA.ape <- function(x,...) {
  res <- list()
  val <- if ("Cum_corr_eig" %in% colnames(x$values)) {
    x$values$Cum_corr_eig
  } else {
    x$values$Cumul_eig
  }
  vars.each <- 100*c(val[1],diff(val))
  vars.cum <- 100*val
  tab <- data.frame(Axis=1:length(vars.each),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCoA.labdsv <- function(x,...) {
  res <- list()
  vars <- x$eig
  vars[vars<0] <- 0
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCoA.stats <- function(x,...) {
  if (!"eig" %in% names(x) || is.null(x$eig)) {
    stop("no eigenvalues available, compute the analysis with 'eig=TRUE'")
  }
  res <- list()
  vars <- x$eig
  vars[vars<0] <- 0
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.PCoA.vegan <- function(x,...) {
  res <- list()
  if ("CA" %in% names(x)) {
    vars <- x$CA$eig
  } else {
    if (!"eig" %in% names(x) || is.null(x$eig)) {
	stop("no eigenvalues available, compute the analysis with 'eig=TRUE'")
    }
    vars <- x$eig
    vars[vars<0] <- 0
  }
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total variance (%)",tab=tab)
  return(res)
}

MVA.get.synt.nMDS.mono.vegan <- function(x,...) {
  res <- list()
  res[[1]] <- list(crit="stress",stress=x$stress)
  return(res)
}

MVA.get.synt.nMDS.iso.vegan <- MVA.get.synt.nMDS.MASS <-
MVA.get.synt.nMDS.labdsv <- function(x,...) {
  res <- list()
  res[[1]] <- list(crit="stress",stress=x$stress/100)
  return(res)
}

MVA.get.synt.COA.ade4 <- MVA.get.synt.MCA.ade4 <- MVA.get.synt.Mix.ade4 <- function(x,...) {
  res <- list()
  vars <- x$eig
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total inertia (%)",tab=tab)
  return(res)
}

MVA.get.synt.COA.vegan <- function(x,...) {
  res <- list()
  vars <- x$CA$eig
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[1]] <- list(crit="total inertia (%)",tab=tab)
  return(res)
}

MVA.get.synt.RDA.ade4 <- function(x,...) {
  res <- list()
  appel <- as.list(x$call)
  dudi <- eval.parent(appel$dudi)
  vars1 <- c(sum(x$eig)/sum(dudi$eig),1-(sum(x$eig)/sum(dudi$eig)))
  vars1.each <- 100*vars1
  vars1.cum <- cumsum(vars1.each)
  tab1 <- data.frame(" "=c("Constrained","Unconstrained"),Proportion=vars1.each,
    Cumulative=vars1.cum,check.names=FALSE)
  res[[1]] <- list(crit="total variance (%)",tab=tab1)
  vars2 <- x$eig
  vars2 <- vars2/sum(vars2)
  vars2.each <- 100*vars2
  vars2.cum <- 100*cumsum(vars2)
  tab2 <- data.frame(Axis=1:length(vars2),Proportion=vars2.each,Cumulative=vars2.cum)
  res[[2]] <- list(crit="constrained variance (%)",tab=tab2)
  return(res)
}

MVA.get.synt.RDAortho.ade4 <- function(x,...) {
  res <- list()
  appel <- as.list(x$call)
  dudi <- eval.parent(appel$dudi)
  vars1 <- c(1-(sum(x$eig)/sum(dudi$eig)),sum(x$eig)/sum(dudi$eig))
  vars1.each <- 100*vars1
  vars1.cum <- cumsum(vars1.each)
  tab1 <- data.frame(" "=c("Constrained","Unconstrained"),Proportion=vars1.each,
    Cumulative=vars1.cum,check.names=FALSE)
  res[[1]] <- list(crit="total variance (%)",tab=tab1)
  vars2 <- x$eig
  vars2 <- vars2/sum(vars2)
  vars2.each <- 100*vars2
  vars2.cum <- 100*cumsum(vars2)
  tab2 <- data.frame(Axis=1:length(vars2),Proportion=vars2.each,Cumulative=vars2.cum)
  res[[2]] <- list(crit="unconstrained variance (%)",tab=tab2)
  return(res)
}

MVA.get.synt.RDA.vegan <- function(x,...) {
  res <- list()
  tot.var <- x$tot.chi
  vars1 <- c(x$pCCA$tot.chi,x$CCA$tot.chi,x$CA$tot.chi)/tot.var
  vars1.each <- 100*vars1
  vars1.cum <- cumsum(vars1.each)
  cols <- c(if(!is.null(x$pCCA$tot.chi)) {"Conditional"},"Constrained","Unconstrained")
  tab1 <- data.frame(" "=cols,Proportion=vars1.each,Cumulative=vars1.cum,check.names=FALSE)
  res[[1]] <- list(crit="total variance (%)",tab=tab1)
  vars2 <- x$CCA$eig/x$CCA$tot.chi
  vars2.each <- 100*vars2
  vars2.cum <- cumsum(vars2.each)
  tab2 <- data.frame(Axis=1:length(vars2.each),Proportion=vars2.each,Cumulative=vars2.cum)
  res[[2]] <- list(crit="constrained variance (%)",tab=tab2)
  vars3 <- x$CA$eig/x$CA$tot.chi
  vars3.each <- 100*vars3
  vars3.cum <- cumsum(vars3.each)
  tab3 <- data.frame(Axis=1:length(vars3.each),Proportion=vars3.each,Cumulative=vars3.cum)
  res[[3]] <- list(crit="unconstrained variance (%)",tab=tab3)
  return(res)
}

MVA.get.synt.CCA.ade4 <- function(x,...) {
  res <- list()
  appel <- as.list(x$call)
  spe <- eval.parent(appel$sitspe)
  coa <- ade4::dudi.coa(spe,scannf=FALSE)
  vars1 <- c(sum(x$eig)/sum(coa$eig),1-(sum(x$eig)/sum(coa$eig)))
  vars1.each <- 100*vars1
  vars1.cum <- cumsum(vars1.each)
  tab1 <- data.frame(" "=c("Constrained","Unconstrained"),Proportion=vars1.each,
    Cumulative=vars1.cum,check.names=FALSE)
  res[[1]] <- list(crit="total inertia (%)",tab=tab1)
  vars2 <- x$eig
  vars2 <- vars2/sum(vars2)
  vars2.each <- 100*vars2
  vars2.cum <- 100*cumsum(vars2)
  tab2 <- data.frame(Axis=1:length(vars2),Proportion=vars2.each,Cumulative=vars2.cum)
  res[[2]] <- list(crit="constrained inertia (%)",tab=tab2)
  return(res)
}

MVA.get.synt.CCA.vegan <- function(x,...) {
  res <- list()
  tot.var <- x$tot.chi
  vars1 <- c(x$pCCA$tot.chi,x$CCA$tot.chi,x$CA$tot.chi)/tot.var
  vars1.each <- 100*vars1
  vars1.cum <- cumsum(vars1.each)
  cols <- c(if(!is.null(x$pCCA$tot.chi)) {"Conditional"},"Constrained","Unconstrained")
  tab1 <- data.frame(" "=cols,Proportion=vars1.each,Cumulative=vars1.cum,check.names=FALSE)
  res[[1]] <- list(crit="total inertia (%)",tab=tab1)
  vars2 <- x$CCA$eig/x$CCA$tot.chi
  vars2.each <- 100*vars2
  vars2.cum <- cumsum(vars2.each)
  tab2 <- data.frame(Axis=1:length(vars2.each),Proportion=vars2.each,Cumulative=vars2.cum)
  res[[2]] <- list(crit="constrained inertia (%)",tab=tab2)
  vars3 <- x$CA$eig/x$CA$tot.chi
  vars3.each <- 100*vars3
  vars3.cum <- cumsum(vars3.each)
  tab3 <- data.frame(Axis=1:length(vars3.each),Proportion=vars3.each,Cumulative=vars3.cum)
  res[[3]] <- list(crit="unconstrained inertia (%)",tab=tab3)
  return(res)
}

MVA.get.synt.dbRDA.vegan <- function(x,...) {
  res <- list()
  tot.var <- if(!is.null(x$CA$imaginary.chi)) {x$tot.chi-x$CA$imaginary.chi} else {x$tot.chi}
  vars1 <- c(x$pCCA$tot.chi,x$CCA$tot.chi,x$CA$tot.chi)/tot.var
  vars1.each <- 100*vars1
  vars1.cum <- cumsum(vars1.each)
  cols <- c(if(!is.null(x$pCCA$tot.chi)) {"Conditional"},"Constrained","Unconstrained")
  tab1 <- data.frame(" "=cols,Proportion=vars1.each,Cumulative=vars1.cum,check.names=FALSE)
  res[[1]] <- list(crit="total variance (%)",tab=tab1)
  vars2 <- x$CCA$eig/x$CCA$tot.chi
  vars2.each <- 100*vars2
  vars2.cum <- cumsum(vars2.each)
  tab2 <- data.frame(Axis=1:length(vars2.each),Proportion=vars2.each,Cumulative=vars2.cum)
  res[[2]] <- list(crit="constrained variance (%)",tab=tab2)
  vars3 <- x$CA$eig/x$CA$tot.chi
  vars3.each <- 100*vars3
  vars3.cum <- cumsum(vars3.each)
  tab3 <- data.frame(Axis=1:length(vars3.each),Proportion=vars3.each,Cumulative=vars3.cum)
  res[[3]] <- list(crit="unconstrained variance (%)",tab=tab3)
  return(res)
}

MVA.get.synt.CCorA.vegan <- function(x,...) {
  res <- list()
  cors <- x$CanCorr
  tab <- data.frame(Axes=1:length(cors),Correlation=cors)
  res[[1]] <- list(crit="correlation between pairs of axes",tab=tab)
  return(res)
}

MVA.get.synt.LDA.MASS <- function(x,...) {
  stop("rather use MVA.cv")
}

MVA.get.synt.LDA.ade4 <- function(x,...) {
  stop("rather use MVA.cv")
}

MVA.get.synt.PLSDA.mixOmics <- function(x,...) {
  stop("rather use MVA.cmv")
}

MVA.get.synt.2BPLS.mixOmics <- function(x,...) {
  res <- list()
  covmat <- cov(x$X,x$Y)
  eig <- svd(covmat)$d
  vars.each <- 100*eig^2/sum(eig^2)
  vars.cum <- cumsum(vars.each)
  tab1 <- data.frame(Axes=1:length(vars.each),Proportion=vars.each,Cumulative=vars.cum)
  tab1 <- tab1[1:x$ncomp,]
  res[[1]] <- list(crit="square covariance (%)",tab=tab1)
  cors <- diag(cor(x$variates$X,x$variates$Y))
  tab2 <- data.frame(Axes=1:length(cors),Correlation=cors)
  res[[2]] <- list(crit="correlation between pairs of axes",tab=tab2)
  return(res)
}

MVA.get.synt.rCCorA.mixOmics <- function(x,...) {
  res <- list()
  cors <- x$cor
  tab <- data.frame(Axes=1:length(cors),Correlation=cors)
  tab <- tab[1:x$ncomp,]
  res[[1]] <- list(crit="correlation between pairs of axes",tab=tab)
  return(res)
}

MVA.get.synt.GPA.FactoMineR <- function(x,...) {
  res <- list()
  PANOVA1 <- x$PANOVA$dimension
  nrow1 <- nrow(PANOVA1)
  vars1.each <- PANOVA1["Total",c("Consensus","residus")]
  vars1.cum <- cumsum(vars1.each)
  tab1 <- data.frame(" "=c("Consensus","Residual"),Proportion=vars1.each,Cumulative=vars1.cum,check.names=FALSE)
  res[[1]] <- list(crit="total variance (%)",tab=tab1)
  vars2.each <- PANOVA1[-nrow1,"Consensus"]
  vars2.cum <- cumsum(vars2.each)
  tab2 <- data.frame(Axis=1:length(vars2.each),Proportion=vars2.each,Cumulative=vars2.cum)
  res[[2]] <- list(crit="total variance (%)",tab=tab2)
  vars3.each <- 100*PANOVA1[-nrow1,"Consensus"]/PANOVA1["Total","Consensus"]
  vars3.cum <- cumsum(vars3.each)
  tab3 <- data.frame(Axis=1:length(vars3.each),Proportion=vars3.each,Cumulative=vars3.cum)
  res[[3]] <- list(crit="consensus variance (%)",tab=tab3)
  PANOVA2 <- x$PANOVA$config
  nrow2 <- nrow(PANOVA2)
  vars4 <- 100*PANOVA2[-nrow2,"SSresidual"]/PANOVA2["sum","SSresidual"]
  tab4 <- data.frame(" "=rownames(PANOVA2)[-nrow2],Proportion=vars4,check.names=FALSE)
  res[[4]] <- list(crit="residual variance (%)",tab=tab4)
  return(res)
}

MVA.get.synt.CPPLS.pls <- MVA.get.synt.PLSR.pls <-
MVA.get.synt.PCR.pls <- function(x,...) {
  res <- list()
  Y.vars <- 100*drop(pls::R2(x,intercept=FALSE,estimate="train")$val)
  if (is.matrix(Y.vars)) {
    crit <- "Y cumulative total variance (%)"
    Y.vars <- as.data.frame(t(Y.vars))
    tot.vars.cum <- rowSums(Y.vars)/ncol(Y.vars)
    tot.vars.each <- c(tot.vars.cum[1],diff(tot.vars.cum))
    Y.tab <- as.data.frame(cbind(Axis=1:nrow(Y.vars),Total.Proportion=round(tot.vars.each,2),
	Total.Cumulative=round(tot.vars.cum,2),round(Y.vars,2)))
  } else {
    crit <- "Y total variance (%)"
    Y.vars.each <- c(Y.vars[1],diff(Y.vars))
    Y.vars.cum <- Y.vars
    Y.tab <- data.frame(Axis=1:length(Y.vars),Proportion=Y.vars.each,Cumulative=Y.vars.cum)
  }
  res[[1]] <- list(crit=crit,tab=Y.tab)
  X.vars <- x$Xvar/x$Xtotvar
  X.vars.each <- 100*X.vars
  X.vars.cum <- 100*cumsum(X.vars)
  X.tab <- data.frame(Axis=1:length(X.vars),Proportion=X.vars.each,Cumulative=X.vars.cum)
  res[[2]] <- list(crit="X total variance (%)",tab=X.tab)
  return(res)
}

MVA.get.synt.PLSR.plsRglm <- function(x,...) {		# !!!! manque la variance de X
  res <- list()
  Y.vars <- as.vector(cor(x$dataY,x$tt,use="pairwise")^2)
  Y.vars.each <- 100*Y.vars
  Y.vars.cum <- 100*cumsum(Y.vars)
  Y.tab <- data.frame(Axis=1:length(Y.vars),Proportion=Y.vars.each,Cumulative=Y.vars.cum)
  res[[1]] <- list(crit="Y total variance (%)",tab=Y.tab)
#  X.vars <- x$Xvar/x$Xtotvar
#  X.vars.each <- 100*X.vars
#  X.vars.cum <- 100*cumsum(X.vars)
#  X.tab <- data.frame(Axis=1:length(X.vars),Proportion=X.vars.each,Cumulative=X.vars.cum)
#  res[[2]] <- list(crit="X total variance (%)",tab=X.tab)
  return(res)
}

MVA.get.synt.rGCCA.mixOmics <- function(x,...) {
  res <- list()
  nblock <- length(x$ncomp)
  maxcomp <- max(x$ncomp)
  sco <- x$variates
  cors <- list()
  for (i in 1:length(sco)) {
    if (ncol(sco[[i]])<maxcomp) {
	while(ncol(sco[[i]])<maxcomp) {sco[[i]] <- cbind(sco[[i]],rep(NA,nrow(sco[[i]])))}
    }
  }
  for (i in 1:maxcomp) {
    tab.cor <- cor(do.call("cbind",lapply(sco,function(x) x[,i])),use="pairwise")
    rownames(tab.cor) <- colnames(tab.cor) <- paste0("Block",1:nblock)
    if(!all(is.na(as.dist(tab.cor)))) {
	cors[[i]] <- list(crit=paste("inter-block correlations - Axes",i),tab=as.dist(tab.cor))
    }
  }
  res[[1]] <- cors
  names(res)[[1]] <- "cors"
  for (i in 1:nblock) {
    vars <- x$AVE$AVE.X[[i]]
    vars.each <- 100*vars
    vars.cum <- cumsum(vars.each)
    tab.vars <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
    res[[i+1]] <- list(crit=paste("intra-block total variance (%) - Block",i),tab=tab.vars)
  }
  return(res)
}

MVA.get.synt.rGCCA.RGCCA <- function(x,...) {
  res <- list()
  nblock <- length(x$ncomp)
  maxcomp <- max(x$ncomp)
  sco <- x$Y
  for (i in 1:length(sco)) {
    if (ncol(sco[[i]])<maxcomp) {
	while(ncol(sco[[i]])<maxcomp) {sco[[i]] <- cbind(sco[[i]],rep(NA,nrow(sco[[i]])))}
    }
  }
  cors <- list()
  for (i in 1:maxcomp) {
    tab.cor <- cor(do.call("cbind",lapply(sco,function(x) x[,i])),use="pairwise")
    rownames(tab.cor) <- colnames(tab.cor) <- paste0("Block",1:nblock)
    if(!all(is.na(as.dist(tab.cor)))) {
	cors[[i]] <- list(crit=paste("inter-block correlations - Axes",i),tab=as.dist(tab.cor))
    }
  }
  res[[1]] <- cors
  names(res)[[1]] <- "cors"
  for (i in 1:nblock) {
    vars <- x$AVE$AVE_X[[i]]
    vars.each <- 100*vars
    vars.cum <- cumsum(vars.each)
    tab.vars <- data.frame(Axis=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
    res[[i+1]] <- list(crit=paste("intra-block total variance (%) - Block",i),tab=tab.vars)
  }
  return(res)
}

MVA.get.synt.CIA.ade4 <- function(x,...) {
  res <- list()
  res$RV <- x$RV
  vars <- x$eig
  vars <- vars/sum(vars)
  vars.each <- 100*vars
  vars.cum <- 100*cumsum(vars)
  tab <- data.frame(Axes=1:length(vars),Proportion=vars.each,Cumulative=vars.cum)
  res[[2]] <- list(crit="co-inertia (%)",tab=tab)
  cors <- diag(cor(x$lX,x$lY))
  tab2 <- data.frame(Axes=1:length(cors),Correlation=cors)
  res[[3]] <- list(crit="correlation between pairs of axes",tab=tab2)
  return(res)
}

MVA.get.synt.PCIA.ade4 <- function(x,...) {
  res <- list()
  X <- scale(x$tabX,scale=FALSE)
  Y <- scale(x$tabY,scale=FALSE)
  var1 <- apply(X,2,function(x) sum(x^2))
  var2 <- apply(Y,2,function(x) sum(x^2))
  tra1 <- sum(var1)
  tra2 <- sum(var2)
  X <- as.matrix(X/sqrt(tra1))
  Y <- as.matrix(Y/sqrt(tra2))
  m2 <- sum(svd(t(X) %*% Y)$d)
  res[[1]] <- list(crit="m2",m2=m2)
  return(res)
}


