byf.normhist <- function(formula,data) {
  if (missing(formula)||(length(formula)!=3)) {stop("missing or incorrect formula")}
  m <- match.call()
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  mf <- eval(m,parent.frame())
  dname <- c(names(mf)[1],paste(names(mf)[2:ncol(mf)],collapse=":"))
  resp <- mf[,1]
  fact <- interaction(mf[,2:ncol(mf)],sep=":")
  nlev <- nlevels(fact)
  couleurs <- rainbow(nlev)
  angles <- seq(20,160,140/(nlev-1))
  dens <- integer()
  for (i in 1:nlev) {
    x.i <- resp[as.numeric(fact)==i]
    dens <- c(dens,hist(x.i,plot=FALSE)$density)
  }
  x.1 <- resp[as.numeric(fact)==1]
  hist(x.1,xlab=dname[1],main="",density=10,angle=angles[1],col=couleurs[1],
    xlim=c(range(resp)*c(0.9,1.1)),ylim=c(0,1.1*max(dens)),freq=FALSE)
  box()
  for (i in 2:nlev) {
    x.i <- resp[as.numeric(fact)==i]
    hist(x.i,xlab="",ylab="",main="",density=10,angle=angles[i],col=couleurs[i],
	freq=FALSE,add=TRUE)
  }
  for (i in 1:nlev) {
    x.i <- resp[as.numeric(fact)==i]
    lines(seq2(resp),dnorm(seq2(resp),mean=mean(x.i),sd=sd(x.i)),col=couleurs[i],lwd=2)
  }
  legend("topright",levels(fact),title=dname[2],lty=1,col=couleurs,lwd=2)
}
