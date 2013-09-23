byf.hist <- function(formula,data,...) {
  if (missing(formula)||(length(formula)!=3)) {stop("missing or incorrect formula")}
  m <- match.call(expand.dots=FALSE)
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m,parent.frame())
  dname <- c(names(mf)[1],paste(names(mf)[2:ncol(mf)],collapse=":"))
  resp <- mf[,1]
  fact <- interaction(mf[,2:ncol(mf)],sep=":")
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  par(mfrow=n2mfrow(nlevels(fact)))
  for (i in 1:nlevels(fact)) {
    hist(resp[as.numeric(fact)==i],xlab=dname[1],main=levels(fact)[i],...)
  }
}
