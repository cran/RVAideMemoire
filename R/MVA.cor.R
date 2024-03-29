# vegan: ordiYbar

MVA.cor <- function(x,xax=1,yax=2,set=c(12,1,2),space=1,...) {
  if (length(set)!=1) {set <- 12}
  if (!set %in% c(12,1,2)) {stop("wrong 'set'")}
  x <- MVA.ident(x)
  class(x) <- unique(class(x))
  corr.temp <- if (inherits(x,c("PCA.ade4","PCA.mixOmics","sPCA.mixOmics","IPCA.mixOmics",
    "sIPCA.mixOmics","LDA.MASS","LDA.ade4","PLSDA.mixOmics","sPLSDA.mixOmics","Multilevel.sPLSDA.mixOmics",
    "CDA.ade4","NSCOA.ade4","CCA.ade4","Mix.ade4","RDAortho.ade4","GPA.FactoMineR"))) {MVA.get.corr(x)} else
    if (inherits(x,c("PLSR.mixOmics","sPLSR.mixOmics","Multilevel.sPLSR.mixOmics","CPPLS.pls","PLSR.pls",
    "PLSR.plsRglm","PLSGLR.plsRglm","PCR.pls","RDA.ade4","PCIA.ade4"))) {MVA.get.corr(x,set)} else
    if (inherits(x,c("PCA.vegan","CCA.vegan","dbRDA.vegan"))) {MVA.get.corr(x,xax,yax)} else
    if (inherits(x,c("RDA.vegan"))) {MVA.get.corr(x,xax,yax,set,space)} else
    if (inherits(x,c("CCorA.vegan","rGCCA.mixOmics","sGCCA.mixOmics","DIABLO.mixOmics",
    "sDIABLO.mixOmics"))) {MVA.get.corr(x,space)} else
    if (inherits(x,c("CIA.ade4","rCCorA.mixOmics","2BPLS.mixOmics","2BsPLS.mixOmics",
    "Multilevel.2BsPLS.mixOmics"))) {MVA.get.corr(x,set,space)} else
    {MVA.get.corr(x)}
  corr <- if (is.data.frame(corr.temp)) {corr.temp} else {corr.temp[[1]]}
  if (!inherits(x,c("PCA.vegan","CCA.vegan","RDA.vegan"))) {
    if (!all(xax %in% c(1:ncol(corr)))) {stop("wrong 'xax'")}
    if (ncol(corr)==1) {
	xax <- 1
	yax <- NULL
    } else if (ncol(corr)>1 & length(xax)>1) {
	yax <- NULL
    }
    if (!is.null(yax) && !yax %in% c(1:ncol(corr))) {
	warning("wrong 'yax', only 'xax' used")
	yax <- NULL
    }
    corrx <- corr[,xax]
    corry <- NULL
    if (!is.null(yax)) {corry <- corr[,yax]}
  } else {
    if (length(xax)>1) {yax <- NULL}
    corrx <- corr[,1:length(xax)]
    corry <- NULL
    if (!is.null(yax) && length(xax)==1 && ncol(corr)==2) {
	corry <- corr[,2]
    } else {
	yax <- NULL
    }
  }
  res.temp <- if (length(xax)==1) {
    as.data.frame(cbind(corrx,corry))
  } else {
    as.data.frame(corrx)
  }
  rownames(res.temp) <- rownames(corr)
  if (inherits(x,c("RDA.vegan"))) {
    colnames(res.temp)[1:length(xax)] <- paste(ifelse(space==1,"Constr. comp.","Unconstr. comp."),xax)
    if (!is.null(yax)) {colnames(res.temp)[2] <- paste(ifelse(space==1,"Constr. comp.","Unconstr. comp."),yax)}
  } else if (inherits(x,c("CCA.vegan","CCA.ade4","RDA.ade4"))) {
    colnames(res.temp)[1:length(xax)] <- paste("Constr. comp.",xax)
    if (!is.null(yax)) {colnames(res.temp)[2] <- paste("Constr. comp.",yax)}
  } else if (inherits(x,"RDAortho.ade4")) {
    colnames(res.temp)[1:length(xax)] <- paste("Unconstr. comp.",xax)
    if (!is.null(yax)) {colnames(res.temp)[2] <- paste("Unconstr. comp.",yax)}
  } else if (inherits(x,c("CCorA.vegan","rCCorA.mixOmics"))) {
    colnames(res.temp)[1:length(xax)] <- paste("Canonical axis",xax)
    if (!is.null(yax)) {colnames(res.temp)[2] <- paste("Canonical axis",yax)}
  } else if (inherits(x,c("CIA.ade4"))) {
    colnames(res.temp)[1:length(xax)] <- paste("Coinertia axis",xax)
    if (!is.null(yax)) {colnames(res.temp)[2] <- paste("Coinertia axis",yax)}
  } else {
    colnames(res.temp)[1:length(xax)] <- paste("Comp.",xax)
    if (!is.null(yax)) {colnames(res.temp)[2] <- paste("Comp.",yax)}
  }
  res <- list(corr=res.temp)
  if (!is.data.frame(corr.temp)) {
    res[[2]] <- corr.temp[[2]]
    names(res)[2] <- names(corr.temp)[2]
  }
  return(res)
}

MVA.get.corr <- function(x,...) {
  UseMethod("MVA.get.corr")
}

MVA.get.corr.default <- MVA.get.corr.unknown <- function(x,...) {
  stop("unknown multivariate analysis or no correlation available")
}

MVA.get.corr.PCA.ade4 <- MVA.get.corr.NSCOA.ade4 <- function(x,...) {as.data.frame(x$co)}

MVA.get.corr.PCA.mixOmics <- MVA.get.corr.IPCA.mixOmics <- 
function(x,...) {as.data.frame(cor(x$X,x$x,use="pairwise"))}

MVA.get.corr.PCA.vegan <- function(x,xax,yax,...) {
  sco <- MVA.scores(x,xax,yax,scaling=1)$coord
  tab <- as.data.frame(cor(vegan::ordiYbar(x,"CA"),sco,use="pairwise"))
  return(tab)
}

MVA.get.corr.sPCA.mixOmics <- function(x,...) {
  keep.X <- apply(abs(x$rotation),1,sum)>0
  cord.X <- as.data.frame(cor(x$X[,keep.X],x$x,use="pairwise"))
  return(cord.X)
}

MVA.get.corr.sIPCA.mixOmics <- function(x,...) {
  keep.X <- apply(abs(x$loadings),1,sum)>0
  cord.X <- as.data.frame(cor(x$X[,keep.X],x$x,use="pairwise"))
  return(cord.X)
}

MVA.get.corr.LDA.MASS <- function(x,...) {as.data.frame(LDA.format(x)$co)}

MVA.get.corr.LDA.ade4 <- MVA.get.corr.CDA.ade4 <- function(x,...) {as.data.frame(x$va)}

MVA.get.corr.PLSDA.mixOmics <- function(x,...) {as.data.frame(cor(x$X,x$variates$X,use="pairwise"))}

MVA.get.corr.sPLSDA.mixOmics <- MVA.get.corr.Multilevel.sPLSDA.mixOmics <- function(x,...) {
  keep.X <- apply(abs(x$loadings$X),1,sum)>0
  cord.X <- as.data.frame(cor(x$X[,keep.X],x$variates$X,use="pairwise"))
  return(cord.X)
}

MVA.get.corr.PLSR.mixOmics <- function(x,set,...) {
  if (set==1) {
    tab <- as.data.frame(cor(x$X,x$variates$X,use="pairwise"))
  } else if (set==2) {
    tab <- cor(x$Y,x$variates$X,use="pairwise")
    if (is.null(rownames(tab))) {rownames(tab) <- x$names$Y}
    tab <- as.data.frame(tab)
  } else {
    X <- cor(x$X,x$variates$X,use="pairwise")
    Y <- cor(x$Y,x$variates$X,use="pairwise")
    if (is.null(rownames(Y))) {
	rownames(X) <- x$names$X
	rownames(Y) <- x$names$Y
    }
    colnames(Y) <- colnames(X)
    tab <- as.data.frame(rbind(X,Y))
  }
  if (set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.sPLSR.mixOmics <- MVA.get.corr.Multilevel.sPLSR.mixOmics <- function(x,set,...) {
  keep.X <- apply(abs(x$loadings$X),1,sum)>0
  keep.Y <- apply(abs(x$loadings$Y),1,sum)>0
  if (set==1) {
    tab <- as.data.frame(cor(x$X[,keep.X],x$variates$X,use="pairwise"))
  } else if (set==2) {
    tab <- cor(x$Y[,keep.Y],x$variates$X,use="pairwise")
    if (is.null(rownames(tab))) {rownames(tab) <- x$names$Y}
    tab <- as.data.frame(tab)
  } else {
    X <- cor(x$X[,keep.X],x$variates$X,use="pairwise")
    Y <- cor(x$Y[,keep.Y],x$variates$X,use="pairwise")
    if (is.null(rownames(Y))) {
	rownames(X) <- x$names$X
	rownames(Y) <- x$names$Y
    }
    colnames(Y) <- colnames(X)
    tab <- as.data.frame(rbind(X,Y))
  }
  if (set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.PLSR.plsRglm <- function(x,set,...) {
  sco <- as.data.frame(x$tt)
  if (set==1) {
    tab <- as.data.frame(cor(x$dataX,sco,use="pairwise"))
  } else if (set==2) {
    tab <- cor(x$dataY,sco,use="pairwise")
    if (is.null(rownames(tab))) {
	rown <- as.character(x$call$dataY)
	rownames(tab) <- rown[length(rown)]
    }
    tab <- as.data.frame(tab)
  } else {
    X <- cor(x$dataX,sco,use="pairwise")
    Y <- cor(x$dataY,sco,use="pairwise")
    if (is.null(rownames(Y))) {
	rown <- as.character(x$call$dataY)
	rownames(Y) <- rown[length(rown)]
    }
    colnames(Y) <- colnames(X)
    tab <- as.data.frame(rbind(X,Y))
  }
  if (set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.PLSGLR.plsRglm <- function(x,set,...) {
  sco <- as.data.frame(x$tt)
  if (set==1) {
    tab <- as.data.frame(cor(x$dataX,sco,use="pairwise"))
  } else if (set==2) {
    tab <- cor(fitted(x$FinalModel)+residuals(x$FinalModel),sco,use="pairwise")
    if (is.null(rownames(tab))) {
	rown <- as.character(x$call$dataY)
	rownames(tab) <- rown[length(rown)]
    }
    tab <- as.data.frame(tab)
  } else {
    X <- cor(x$dataX,sco,use="pairwise")
    Y <- cor(fitted(x$FinalModel)+residuals(x$FinalModel),sco,use="pairwise")
    if (is.null(rownames(Y))) {
	rown <- as.character(x$call$dataY)
	rownames(Y) <- rown[length(rown)]
    }
    colnames(Y) <- colnames(X)
    tab <- as.data.frame(rbind(X,Y))
  }
  if (set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.CPPLS.pls <- MVA.get.corr.PLSR.pls <- MVA.get.corr.PCR.pls <- 
function(x,set,...) {
  if (!"model" %in% names(x)) {stop("compute the analysis with 'model=TRUE'")}
  sco <- cbind(x$scores)
  if (set==1) {
    X <- x$model[,2]
    tab <- cor(X,sco,use="pairwise")
    if (is.null(rownames(tab)) && !is.null(colnames(x$model[,2]))) {rownames(tab) <- colnames(x$model[,2])}
    tab <- as.data.frame(tab)
  } else if (set==2) {
    Y <- x$model[,1]
    tab <- cor(Y,sco,use="pairwise")
    if (is.null(rownames(tab))) {
	coln <- colnames(as.matrix(x$model[,1],nrow=nrow(sco)))
	if (!is.null(coln)) {rownames(tab) <- coln}
    }
    tab <- as.data.frame(tab)
  } else {
    X <- x$model[,2]
    Y <- x$model[,1]
    coX <- cor(X,sco,use="pairwise")
    if (is.null(rownames(coX)) && !is.null(colnames(x$model[,2]))) {rownames(coX) <- colnames(x$model[,2])}
    coY <- cor(Y,sco,use="pairwise")
    if (is.null(rownames(coY))) {
	coln <- colnames(as.matrix(x$model[,1],nrow=nrow(sco)))
	if (!is.null(coln)) {rownames(coY) <- coln}
    }
    colnames(coY) <- colnames(coX)
    tab <- as.data.frame(rbind(coX,coY))
  }
  if (set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(coX),nrow(coY))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.CCA.vegan <- function(x,xax,yax,...) {
  sco <- MVA.scores(x,xax,yax,scaling=1,set=1)$coord
  indep.var <- if ("formula" %in% names(x$call)) {
    as.data.frame(model.frame(x))
  } else {
    as.data.frame(eval(x$call$Y))
  }
  type <- logical(ncol(indep.var))
  for (i in 1:ncol(indep.var)) {type[i] <- is.numeric(indep.var[,i])}
  if (all(!type)) {stop("only factor constraints, no correlation")}
  if (!all(type)) {indep.var <- indep.var[,type]}
  tab <- cor(indep.var,sco,use="pairwise")
  if (is.null(rownames(tab))) {
    rown <- as.character(x$call$Y)
    rownames(tab) <- rown[length(rown)]
  }
  tab <- as.data.frame(tab)
  return(tab)
}

MVA.get.corr.CCA.ade4 <- function(x,...) {
  sco <- as.data.frame(x$li)
  indep.var <- as.data.frame(eval(x$call$sitenv))
  type <- logical(ncol(indep.var))
  for (i in 1:ncol(indep.var)) {type[i] <- is.numeric(indep.var[,i])}
  if (all(!type)) {stop("only factor constraints, no correlation")}
  if (!all(type)) {indep.var <- indep.var[,type]}
  tab <- as.data.frame(cor(indep.var,sco,use="pairwise"))
  return(tab)
}

MVA.get.corr.Mix.ade4 <- function(x,...) {
  index <- x$index=="q"
  num <- match(seq(1,length(x$index))[index],x$assign)
  as.data.frame(x$co[num,])
}

MVA.get.corr.RDA.vegan <- function(x,xax,yax,set,space,...) {
  if (!space %in% c(1,2)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 1}
  if (space==2 & set!=2) {
    set <- 2
    warning("'set' re-set to 2 since 'set=1' (or '12') does not make sense in the unconstrained space")
  }
  sco <- MVA.scores(x,xax,yax,scaling=1,space=space)$coord
  numX <- TRUE
  if (set==1) {
    indep.var <- if ("formula" %in% names(x$call)) {
	as.data.frame(model.frame(x))
    } else {
	as.data.frame(eval(x$call$Y))
    }
    type <- logical(ncol(indep.var))
    for (i in 1:ncol(indep.var)) {type[i] <- is.numeric(indep.var[,i])}
    if (all(!type)) {stop("only factor constraints, no correlation")}
    indep.var.names <- colnames(indep.var)[type]
    if (!all(type)) {indep.var <- indep.var[,type]}
    tab <- cor(indep.var,sco,use="pairwise")
    if (is.null(rownames(tab))) {
	rownames(tab) <- indep.var.names
    }
    tab <- as.data.frame(tab)
  } else if (set==2) {
    dep.var <- if (space==1) {vegan::ordiYbar(x,"partial")} else {vegan::ordiYbar(x,"CA")}
    tab <- as.data.frame(cor(dep.var,sco,use="pairwise"))
  } else {
    dep.var <- if (space==1) {vegan::ordiYbar(x,"partial")} else {vegan::ordiYbar(x,"CA")}
    indep.var <- if ("formula" %in% names(x$call)) {
	as.data.frame(model.frame(x))
    } else {
	as.data.frame(eval(x$call$Y))
    }
    type <- logical(ncol(indep.var))
    for (i in 1:ncol(indep.var)) {type[i] <- is.numeric(indep.var[,i])}
    if (all(!type)) {
	tab <- as.data.frame(cor(dep.var,sco,use="pairwise"))
	numX <- FALSE
    } else {
	indep.var.names <- colnames(indep.var)[type]
	if (!all(type)) {indep.var <- indep.var[,type]}
	X <- cor(indep.var,sco,use="pairwise")
	Y <- cor(dep.var,sco,use="pairwise")
	if (is.null(rownames(X))) {
	  rownames(X) <- indep.var.names
	}
	colnames(Y) <- colnames(X)
	tab <- as.data.frame(rbind(X,Y))
    }
  }
  if (set==12 & numX) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.RDA.ade4 <- function(x,set,...) {
  sco <- as.data.frame(x$li)
  numX <- TRUE
  if (set==1) {
    indep.var <- as.data.frame(eval(x$call$df))
    type <- logical(ncol(indep.var))
    for (i in 1:ncol(indep.var)) {type[i] <- is.numeric(indep.var[,i])}
    if (all(!type)) {stop("only factor constraints, no correlation")}
    if (!all(type)) {indep.var <- indep.var[,type]}
    tab <- as.data.frame(cor(indep.var,sco,use="pairwise"))
  } else if (set==2) {
    tab <- as.data.frame(cor(x$Y,sco,use="pairwise"))
  } else {
    indep.var <- as.data.frame(eval(x$call$df))
    type <- logical(ncol(indep.var))
    for (i in 1:ncol(indep.var)) {type[i] <- is.numeric(indep.var[,i])}
    if (all(!type)) {
	tab <- as.data.frame(cor(x$Y,sco,use="pairwise"))
	numX <- FALSE
    } else {
	if (!all(type)) {indep.var <- indep.var[,type]}
	X <- cor(indep.var,sco,use="pairwise")
	Y <- cor(x$Y,sco,use="pairwise")
	colnames(Y) <- colnames(X)
	tab <- as.data.frame(rbind(X,Y))
    }
  }
  if (set==12 & numX) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.RDAortho.ade4 <- function(x,...) {
  as.data.frame(cor(eval(eval(x$call$dudi)$call$df),x$li,use="pairwise"))
}

MVA.get.corr.dbRDA.vegan <- function(x,xax,yax,...) {
  sco <- MVA.scores(x,xax,yax,scaling=1,space=1)$coord
  indep.var <- as.data.frame(model.frame(x))
  type <- logical(ncol(indep.var))
  for (i in 1:ncol(indep.var)) {type[i] <- is.numeric(indep.var[,i])}
  if (all(!type)) {stop("only factor constraints, no correlation")}
  indep.var.names <- colnames(indep.var)[type]
  if (!all(type)) {indep.var <- indep.var[,type]}
  tab <- cor(indep.var,sco,use="pairwise")
  if (is.null(rownames(tab))) {
    rownames(tab) <- indep.var.names
  }
  tab <- as.data.frame(tab)
  res <- tab
  return(res)
}

MVA.get.corr.CCorA.vegan <- function(x,space,...) {
  if (!space %in% c(1,2)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 1}
  if (space==1) {
    res <- as.data.frame(x$corr.X.Cx)
  } else {
    res <- as.data.frame(x$corr.Y.Cy)
  }
  return(res)
}

MVA.get.corr.CIA.ade4 <- function(x,set,space,...) {
  if (!space %in% c(1,2,3)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 1}
  if (space==1) {
    tab.raw <- eval(eval(x$call$dudiX)$call$df)
    type <- logical(ncol(tab.raw))
    for (i in 1:ncol(tab.raw)) {type[i] <- !is.factor(tab.raw[,i])}
    tab <- as.data.frame(cor(tab.raw[,type],x$lX,use="pairwise"))
  } else if (space==2) {
    tab.raw <- eval(eval(x$call$dudiY)$call$df)
    type <- logical(ncol(tab.raw))
    for (i in 1:ncol(tab.raw)) {type[i] <- !is.factor(tab.raw[,i])}
    tab <- as.data.frame(cor(tab.raw[,type],x$lY,use="pairwise"))
  } else {
    if (set==1) {
	tab.raw <- eval(eval(x$call$dudiX)$call$df)
	type <- logical(ncol(tab.raw))
	for (i in 1:ncol(tab.raw)) {type[i] <- !is.factor(tab.raw[,i])}
	tab <- as.data.frame(cor(tab.raw[,type],x$mX,use="pairwise"))
    } else if (set==2) {
	tab.raw <- eval(eval(x$call$dudiY)$call$df)
	type <- logical(ncol(tab.raw))
	for (i in 1:ncol(tab.raw)) {type[i] <- !is.factor(tab.raw[,i])}
	tab <- as.data.frame(cor(tab.raw[,type],x$mY,use="pairwise"))
    } else {
	X.raw <- eval(eval(x$call$dudiX)$call$df)
	X.type <- logical(ncol(X.raw))
	for (i in 1:ncol(X.raw)) {X.type[i] <- !is.factor(X.raw[,i])}
	X <- cor(X.raw[,type],x$mX,use="pairwise")
	Y.raw <- eval(eval(x$call$dudiY)$call$df)
	Y.type <- logical(ncol(Y.raw))
	for (i in 1:ncol(Y.raw)) {Y.type[i] <- !is.factor(Y.raw[,i])}
	Y <- cor(Y.raw[,Y.type],x$mY,use="pairwise")
	colnames(Y) <- colnames(X)
	if (any(rownames(X) %in% rownames(Y))) {
	  rownames(X) <- paste0("X.",rownames(X))
	  rownames(Y) <- paste0("Y.",rownames(Y))
	}
	tab <- as.data.frame(rbind(X,Y))
    }
  }
  if (space==3 && set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.PCIA.ade4 <- function(x,set,...) {
  X <- x$tabX
  Y <- x$tabY
  sco.X <- x$scorX
  sco.Y <- x$scorY
  if (set==1) {
    tab <- as.data.frame(cor(X,sco.X,use="pairwise"))
  } else if (set==2) {
    tab <- as.data.frame(cor(Y,sco.Y,use="pairwise"))
  } else {
    tab.X <- cor(X,sco.X,use="pairwise")
    tab.Y <- cor(Y,sco.Y,use="pairwise")
    colnames(tab.Y) <- colnames(tab.X)
    if (any(rownames(tab.X) %in% rownames(tab.Y))) {
	rownames(tab.X) <- paste0("X.",rownames(tab.X))
	rownames(tab.Y) <- paste0("Y.",rownames(tab.Y))
    }
    tab <- as.data.frame(rbind(tab.X,tab.Y))
  }
  if (set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(tab.X),nrow(tab.Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.GPA.FactoMineR <- function(x,...) {
  cor.list <- x$correlations
  if ("averagecor" %in% names(cor.list)) {
    to.keep <- (1:length(cor.list))[-which(names(cor.list)=="averagecor")]
    cor.list2 <- list()
    for (i in 1:length(to.keep)) {cor.list2[[i]] <- cor.list[[to.keep[i]]]}
    cor.list <- cor.list2
  }
  res <- do.call("rbind",cor.list)
  res <- as.data.frame(res,row.names=1:nrow(res))
  rown <- c(NULL)
  for (i in 1:length(cor.list)) {
    rown <- c(rown,paste0(rownames(x$RV)[i],".V",1:nrow(cor.list[[i]])))
  }
  rownames(res) <- rown
  return(res)
}

MVA.get.corr.2BPLS.mixOmics <- MVA.get.corr.rCCorA.mixOmics <- function(x,set,space,...) {
  if (!space %in% c(1,2,3)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 3}
  if (space==1) {
    sco <- x$variates$X
    tab <- as.data.frame(cor(x$X,sco,use="pairwise"))
  } else if (space==2) {
    sco <- x$variates$Y
    tab <- as.data.frame(cor(x$Y,sco,use="pairwise"))
  } else {
    sco <- (x$variates$X+x$variates$Y)/2
    if (set==1) {
	tab <- as.data.frame(cor(x$X,sco,use="pairwise"))
    } else if (set==2) {
	tab <- as.data.frame(cor(x$Y,sco,use="pairwise"))
    } else {
	X <- cor(x$X,sco,use="pairwise")
	Y <- cor(x$Y,sco,use="pairwise")
	colnames(Y) <- colnames(X)
	if (any(rownames(X) %in% rownames(Y))) {
	  rownames(X) <- paste0("X.",rownames(X))
	  rownames(Y) <- paste0("Y.",rownames(Y))
	}
	tab <- as.data.frame(rbind(X,Y))
    }
  }
  if (space==3 && set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.2BsPLS.mixOmics <- MVA.get.corr.Multilevel.2BsPLS.mixOmics <- function(x,set,space,...) {
  if (!space %in% c(1,2,3)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 3}
  keep.X <- apply(abs(x$loadings$X),1,sum)>0
  keep.Y <- apply(abs(x$loadings$Y),1,sum)>0
  if (space==1) {
    sco <- x$variates$X
    tab <- as.data.frame(cor(x$X,sco,use="pairwise"))[keep.X,]
  } else if (space==2) {
    sco <- x$variates$Y
    tab <- as.data.frame(cor(x$Y,sco,use="pairwise"))[keep.Y,]
  } else {
    sco <- (x$variates$X+x$variates$Y)/2
    if (set==1) {
	tab <- as.data.frame(cor(x$X,sco,use="pairwise"))[keep.X,]
    } else if (set==2) {
	tab <- as.data.frame(cor(x$Y,sco,use="pairwise"))[keep.Y,]
    } else {
	X <- cor(x$X,sco,use="pairwise")[keep.X,]
	Y <- cor(x$Y,sco,use="pairwise")[keep.Y,]
	colnames(Y) <- colnames(X)
	if (any(rownames(X) %in% rownames(Y))) {
	  rownames(X) <- paste0("X.",rownames(X))
	  rownames(Y) <- paste0("Y.",rownames(Y))
	}
	tab <- as.data.frame(rbind(X,Y))
    }
  }
  if (space==3 && set==12) {
    res <- list(corr=tab)
    res$set <- factor(rep(c("X","Y"),c(nrow(X),nrow(Y))))
  } else {res <- tab}
  return(res)
}

MVA.get.corr.rGCCA.mixOmics <- function(x,space,...) {
  if (!space %in% 1:length(x$variates)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 1}
  as.data.frame(cor(x$X[[space]],x$variates[[space]],use="pairwise"))
}

MVA.get.corr.sGCCA.mixOmics <- function(x,space,...) {
  if (!space %in% 1:length(x$variates)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 1}
  keep.X <- apply(abs(x$loadings[[space]]),1,sum)>0
  as.data.frame(cor(x$X[[space]],x$variates[[space]],use="pairwise")[keep.X,])
}

MVA.get.corr.DIABLO.mixOmics <- function(x,space,...) {
  if (!space %in% 1:(length(x$variates)-1)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 1}
  as.data.frame(cor(x$X[[space]],x$variates[[space]],use="pairwise"))
}

MVA.get.corr.sDIABLO.mixOmics <- function(x,space,...) {
  if (!space %in% 1:(length(x$variates)-1)) {stop("wrong 'space'")}
  if (length(space)!=1) {space <- 1}
  keep.X <- apply(abs(x$loadings[[space]]),1,sum)>0
  as.data.frame(cor(x$X[[space]],x$variates[[space]],use="pairwise")[keep.X,])
}



