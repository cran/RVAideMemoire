# ade4: as.dudi, dist.quant

predict.coadisc <-
function(object,newdata,method=c("mahalanobis", "euclidian"),...) {
  method <- match.arg(method)
  df <- if ("df" %in% names(object)) {
    object$df
  } else {
    eval.parent(object$call$df)
  }
  if (ncol(df)!=ncol(newdata)) {stop("incorrect dimensions of 'newdata'")}
  ncomp <- object$nf
  N <- sum(df)
  df <- df/N
  col.w <- colSums(df)
  fa <- as.matrix(object$fa)
  coord <- matrix(0,ncol=ncomp,nrow=nrow(newdata))
  colnames(coord) <- paste0("DS",1:ncol(coord))
  newdata <- as.matrix(newdata)
  for (i in 1:nrow(newdata)) {
    new <- t(newdata[i,])/N
    row.w.new <- apply(new,1,sum)
    new <- new/row.w.new
    new <- sweep(new,2,col.w)
    X <- ade4::as.dudi(as.data.frame(new),1/col.w,row.w.new,scannf=FALSE,nf=ncomp, 
	call=match.call(),type="coarp",full=TRUE)
    tab <- as.matrix(X$tab)
    coord[i,] <- tab%*%fa
  }
  Y <- if ("fac" %in% names(object)) {
    object$fac
  } else {
    eval.parent(object$call$fac)
  }
  centr <- apply(object$li,2,function(x) tapply(x,Y,mean))
  res <- character(nrow(coord))
  for (i in 1:nrow(coord)) {
    r <- rbind(coord[i,],centr)
    dis <- if (method=="mahalanobis") {
	as.matrix(ade4::dist.quant(r,method=3))[-1,]
    } else {
	as.matrix(dist(r,method="euclidian"))[-1,]
    }
    res[i] <- rownames(dis)[which.min(dis[,1])]
  }
  res <- factor(res)
  return(res)
}
