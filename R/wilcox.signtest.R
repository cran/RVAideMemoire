wilcox.signtest <-
function (x,data,mu=0) {
  if (is(x,"formula")) {
    if (length(x)!=3) {stop("missing or incorrect formula")}
    m <- match.call()
    if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
    m[[1]] <- as.name("model.frame")
    names(m)[2] <- "formula"
    m$mu <- NULL
    mf <- eval(m,parent.frame())
    dname <- paste(names(mf)[1],paste(names(mf)[2:ncol(mf)],collapse=":"),sep=" by ")
    resp <- mf[,1]
    fact <- interaction(mf[,2:ncol(mf)],sep=":")
    ech1 <- resp[as.numeric(fact)==1]
    ech2 <- resp[as.numeric(fact)==2]
    if (length(ech1)!=length(ech2)) {
	stop(paste("'",levels(fact)[1],"' and '",levels(fact)[2],"' lengths differ",sep=""))
    }
    ech1 <- ech1[complete.cases(ech1,ech2)]
    ech2 <- ech2[complete.cases(ech1,ech2)]
    signs <- ech1-ech2
    p <- binom.test(length(signs[signs>0]),length(signs[signs!=0]),0.5)$p.value
    med <- tapply(resp,fact,median,na.rm=TRUE)
    names(med) <- paste("median in group ",levels(fact),sep="")
    names(mu) <- "difference in medians"
  } else {
    if (!is.numeric(mu)) {
	stop("'mu' is missing")
    } else {
	dname <- paste(deparse(substitute(x)),sep="")
	x <- na.omit(x)
	signs <- x-mu
	p <- binom.test(length(signs[signs>0]),length(signs[signs!=0]),0.5)$p.value
	med <- median(x)
	names(med) <- "median of x"
	names(mu) <- "median"
    }
  }
  result <- list(method="Wilcoxon sign test",data.name=dname,null.value=mu,p.value=p,estimate=med,
    alternative="two.sided")
  class(result) <- "htest"
  return(result)
}
