# $tab: data
# $lw: row weights
# $rank: number of axes
# $cw: column weights
# $eig: eigenvalues
# $c1: the column normed scores i.e. principal axes

to.dudi <- function(ord) {
  ord <- MVA.ident(ord)
  res <- convert.to.dudi(ord)
  return(res)
}

convert.to.dudi <- function(x,...) {
  UseMethod("convert.to.dudi")
}

convert.to.dudi.default <- function(x,...) {
  stop("unkonwn multivariate analysis")
}

convert.to.dudi.PCA.vegan <- function(x,...) {
  res <- list()
  tab <- x$CA$Xbar
  if (!is.null(attr(tab,"scaled:scale"))) {
    for (i in 1:ncol(tab)) {tab[,i] <- tab[,i]*attr(tab,"scaled:scale")[i]}
    tab <- scale(tab,center=FALSE,scale=apply(tab,2,function(x) sqrt(sum(x*x)/nrow(tab))))
  }
  res$tab <- as.data.frame(tab)
  res$rank <- x$CA$rank
  res$lw <- rep(1/nrow(res$tab),nrow(res$tab))
  res$cw <- rep(1,ncol(res$tab))
  call <- x$call
  for (i in 1:length(call)) {
    if (!names(call)[i] %in% c("","X")) {call[[i]] <- NULL}
  }
  call$scannf <- FALSE
  call$nf <- res$rank
  names(call)[which(names(call)=="X")] <- "df"
  call[[1]] <- as.name("dudi.pca")
  res$call <- call
  res$nf <- res$rank
  res$eig <- x$CA$eig
  res$c1 <- x$CA$v
  rownames(res$c1) <- colnames(res$tab)
  class(res) <- c("dudi.pca","dudi")
  return(res)
}

convert.to.dudi.PCoA.vegan <- function(x,...) {
  res <- list()
  if (inherits(x,"wcmdscale")) {
    res$tab <- as.data.frame(x$points)
    if (ncol(res$tab)<length(x$eig)) {
	x$call$k <- length(x$eig)
	x2 <- eval(x$call)
	res$tab <- as.data.frame(x2$points)
    }
    res$rank <- length(x$eig)
    res$lw <- x$weights/sum(x$weights)
    res$nf <- ncol(x$points)
    res$eig <- x$eig/sum(x$weights)
    call <- x$call
    for (i in 1:length(call)) {
	if (!names(call)[i] %in% c("","d")) {call[[i]] <- NULL}
    }
    call$scannf <- FALSE
    call$nf <- res$rank
    call[[1]] <- as.name("dudi.pco")
    res$call <- call
  } else {
    tab.0 <- x$CA$u
    std <- apply(tab.0,2,sd)
    sqeig <- sqrt(x$CA$eig)
    tab <- tab.0
    for (i in 1:ncol(tab)) {tab[,i] <- tab[,i]/std[i]*sqeig[i]}
    res$tab <- as.data.frame(tab)
    res$rank <- x$CA$rank
    res$lw <- rep(1/nrow(res$tab),nrow(res$tab))
    res$nf <- res$rank
    res$eig <- x$CA$eig*(nrow(tab)-1)/nrow(tab)
    call <- x$call
    for (i in 1:length(call)) {
	if (!names(call)[i] %in% c("","formula")) {call[[i]] <- NULL}
    }
    call$d <- call$formula[[2]]
    call$formula <- NULL
    call$scannf <- FALSE
    call$nf <- res$rank
    call[[1]] <- as.name("dudi.pco")
    res$call <- call
  }
  res$cw <- rep(1,ncol(res$tab))
  c1 <- diag(1,length(res$eig))
  rownames(c1) <- paste0("A",1:length(res$eig))
  colnames(c1) <- paste0("CS",1:ncol(c1))
  c1 <- c1[,1:res$nf]
  if (res$nf==1) {
    c1 <- as.data.frame(c1)
    colnames(c1) <- "CS1"
  }
  res$c1 <- c1
  class(res) <- c("dudi.pco","dudi")
  return(res)
}

convert.to.dudi.COA.vegan <- function(x,...) {
  res <- list()
  tab <- eval(x$call$X)
  tab <- tab/sum(tab)
  rowS <- rowSums(tab)
  colS <- colSums(tab)
  tab <- tab/rowS
  tab <- sweep(tab,2,colS,"/")-1
  res$tab <- as.data.frame(tab)
  res$rank <- x$CA$rank
  res$lw <- rowS
  res$cw <- colS
  call <- x$call
  for (i in 1:length(call)) {
    if (!names(call)[i] %in% c("","X")) {call[[i]] <- NULL}
  }
  call$scannf <- FALSE
  call$nf <- res$rank
  names(call)[which(names(call)=="X")] <- "df"
  call[[1]] <- as.name("dudi.coa")
  res$call <- call
  res$nf <- res$rank
  res$eig <- x$CA$eig
  res$c1 <- x$CA$v
  class(res) <- c("dudi.coa","dudi")
  return(res)

}
