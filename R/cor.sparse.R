cor.sparse <- function(x) {
  keep.X <- if (inherits(x,"spca")) {
    apply(abs(x$rotation),1,sum)>0
  } else if (inherits(x,"sipca")) {
    apply(abs(x$loadings),1,sum)>0
  } else if (inherits(x,"splsda")) {
    apply(abs(x$loadings$X),1,sum) > 0
  }
  cord.X <- if (inherits(x,"spca")) {
    cor(x$X[,keep.X],x$x,use="pairwise")
  } else if (inherits(x,"sipca")) {
    cor(x$X[,keep.X],x$x,use="pairwise")
  } else if (inherits(x,"splsda")) {
    cor(x$X[,keep.X],x$variates$X,use="pairwise")
  }
  return(cord.X)
}

