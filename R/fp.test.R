fp.test <- function(x,...) {
  UseMethod("fp.test")
}

fp.test.default <- function (x,y,delta=0,alternative="two.sided",...) {
  dname <- paste(deparse(substitute(x)),"and",deparse(substitute(y)))
  names(delta) <- "difference in location"
  y <- y - delta
  Px <- sapply(x,function(z) sum(y<z)+0.5*sum(y==z))
  Py <- sapply(y,function(z) sum(x<z)+0.5*sum(x==z))
  Sx <- sum(Px)
  Sy <- sum(Py)
  Mx <- mean(Px)
  My <- mean(Py)
  Vx <- sum((Px-Mx)^2)
  Vy <- sum((Py-My)^2)
  z <- (Sy-Sx)/(2*sqrt(Vx+Vy+Mx*My))
  if (alternative=="greater") {
    pval <- 1-pnorm(z)
  } else if (alternative=="less") {
    pval <- pnorm(z)
  } else if (alternative=="two.sided") {
    pval <- min(pnorm(z),pnorm(z,lower.tail=FALSE))*2
  }
  result <- list(statistic=z,p.value=pval,alternative=alternative,method="Fligner-Policello test",
    data.name=dname,null.value=delta)
  class(result) <- "htest"
  return(result)
}

fp.test.formula <- function(formula,data,subset,...) {
  if (missing(formula) || (length(formula)!=3L) || (length(attr(terms(formula[-2L]), 
      "term.labels"))!=1L)) {stop("'formula' missing or incorrect")}
  m <- match.call(expand.dots=FALSE)
  if (is.matrix(eval(m$data,parent.frame())))  {m$data <- as.data.frame(data)}
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m,parent.frame())
  DNAME <- paste(names(mf),collapse=" by ")
  names(mf) <- NULL
  response <- attr(attr(mf,"terms"),"response")
  g <- factor(mf[[-response]])
  if (nlevels(g)!=2L) {stop("grouping factor must have exactly 2 levels")}
  DATA <- setNames(split(mf[[response]],g),c("x","y"))
  y <- do.call("fp.test",c(DATA,list(...)))
  y$data.name <- DNAME
  y
}
