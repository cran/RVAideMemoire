se <-
function(var) {
  sd(var,na.rm=TRUE)/sqrt(length(na.omit(var)))
}

