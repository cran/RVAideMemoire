se <-
function(x,y=NULL) {
  if (is.null(y)) {
    result <- sd(x,na.rm=TRUE)/sqrt(length(na.omit(x)))
  } else {
    p <- x/y
    result <- sqrt((p*(1-p))/(y-1))
  }
  return(result)
}

