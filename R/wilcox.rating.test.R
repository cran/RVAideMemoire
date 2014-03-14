wilcox.rating.test <- function(x,...) {
  UseMethod("wilcox.rating.test")
}

wilcox.rating.test.default <- function(x,y=NULL,mu=NULL,...) {
  if (is.null(y) & is.null(mu)) {stop("y and mu are NULL: no test to perform")}
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))
  if (!is.factor(x)) {x <- factor(x)}
  if (!is.ordered(x)) {x <- factor(x,ordered=TRUE)}
  if (!is.null(y)) {
    if (!is.factor(y)) {y <- factor(y)}
    if (!is.ordered(y)) {y <- factor(y,ordered=TRUE)}
  }
  if (!is.null(mu)) {
    if (is.null(y)) {
	mu2 <- which(levels(x)==as.character(mu))
	if (length(mu2)==0 || is.na(mu2)) {stop(paste0("'mu' is not a level of ",xname))}
    } else {
	mu2 <- 0
    }
  } else {
    mu2 <- 0
  }
  x2 <- as.numeric(x)
  y2 <- if (!is.null(y)) {
    as.numeric(y)
  } else {
    NULL
  }
  test <- suppressWarnings(wilcox.test(x2,y2,mu=mu2,...))
  test$data.name <- if (!is.null(y)) {
    paste0(xname," and ",yname)
  } else {
    xname
  }
  test$null.value <- ifelse (is.null(y),mu,mu2)
  names(test$null.value) <- ifelse(is.null(y),"rating","location shift")
  if (!is.null(y)) {
    med <- c(median(x),median(y))
    names(med) <- paste0("median of ",c("x","y"))
  } else {
    med <- median(x)
    names(med) <- paste0("median of ",xname)
  }
  test$estimate <- med
  test
}

wilcox.rating.test.formula <- function(formula,data,subset,na.action,...) {
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
  y <- do.call("wilcox.rating.test",c(DATA,list(...)))
  y$data.name <- DNAME
  names(y$estimate) <- paste0("median in group ",levels(g))
  y
}
