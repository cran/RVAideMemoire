kruskal.rating.test <- function(x,...) {
  UseMethod("kruskal.rating.test")
}

kruskal.rating.test.default <- function(x,g,...) {
  xname <- deparse(substitute(x))
  gname <- deparse(substitute(g))
  if (!is.factor(x)) {x <- factor(x)}
  if (!is.ordered(x)) {x <- factor(x,ordered=TRUE)}
  x2 <- as.numeric(x)
  test <- suppressWarnings(kruskal.test(x2,g,...))
  test$data.name <- paste0(xname," by ",gname)
  test
}

kruskal.rating.test.formula <- function(formula,data,subset,na.action,...) {
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
  x <- mf[[response]]
  g <- factor(mf[[-response]])
  y <- kruskal.rating.test(x,g,...)
  y$data.name <- DNAME
  y
}
