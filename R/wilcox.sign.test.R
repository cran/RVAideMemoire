wilcox.sign.test <-
function(x,y,mu=NULL) {
  if (is.null(mu)) {
    if (length(x)!=length(y)) {stop("'x' and 'y' lengths differ")}
    datas<-c(deparse(substitute(x)),deparse(substitute(y)))
    if (any(is.na(x))) {
	y<-y[-which(is.na(x))]
	x<-x[-which(is.na(x))]
    }
    if (any(is.na(y))) {
	x<-x[-which(is.na(y))]
	y<-y[-which(is.na(y))]
    }
    signs<-integer(length(x))
    for (i in 1:length(x)) {
	if (x[i]<y[i]) {signs[i]<-"-"} else
	if (x[i]>y[i]) {signs[i]<-"+"} else {
	  signs[i]<-NA}
    }
    signs2<-na.omit(signs)
    p<-min(pbinom(length(signs2[signs2=="+"]),length(signs2),0.5),
	pbinom(length(signs2[signs2=="+"]),length(signs2),0.5,lower.tail=FALSE))*2
    result<-list(datas=datas,mu=mu,p.value=p)
  } else {
    datas<-c(deparse(substitute(x)))
    if (any(is.na(x))) {x<-x[-which(is.na(x))]}
    signs<-integer(length(x))
    for (i in 1:length(x)) {
	if (x[i]<mu) {signs[i]<-"-"} else
	if (x[i]>mu) {signs[i]<-"+"} else {
	  signs[i]<-NA}
    }
    signs2<-na.omit(signs)
    p<-min(pbinom(length(signs2[signs2=="+"]),length(signs2),0.5),
	pbinom(length(signs2[signs2=="+"]),length(signs2),0.5,lower.tail=FALSE))*2
    result<-list(datas=datas,mu=mu,p.value=p)
  }
  class(result)<-c("wilcox.sign.test","list")
  return(result)
}

