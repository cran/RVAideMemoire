stand <- function(tab,ref.tab=NULL,center=NULL,scale=NULL) {
  tab <- as.matrix(tab)
  center <- if(!is.null(ref.tab)) {
    if (!is.null(attr(ref.tab,"scaled:center"))) {
	attr(ref.tab,"scaled:center")
    } else {
	colMeans(ref.tab,na.rm=TRUE)
    }
  } else {
    if (!is.null(center)) {
	center
    } else {
	colMeans(tab,na.rm=TRUE)
    }
  }
  scale <- if(!is.null(ref.tab)) {
    if (!is.null(attr(ref.tab,"scaled:scale"))) {
	attr(ref.tab,"scaled:scale")
    } else {
	apply(ref.tab,2,sd,na.rm=TRUE)
    }
  } else {
    if (!is.null(scale)) {
	scale
    } else {
	apply(tab,2,sd,na.rm=TRUE)
    }
  }
  tab <- scale(tab,center=center,scale=scale)
  return(tab)
}
