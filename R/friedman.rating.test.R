friedman.rating.test <- function(y,...) {
  UseMethod("friedman.rating.test")
}

friedman.rating.test.default <- function(y,groups,blocks,...) {
  yname <- deparse(substitute(y))
  gname <- deparse(substitute(groups))
  bname <- deparse(substitute(blocks))
  if (!is.factor(y)) {y <- factor(y)}
  if (!is.ordered(y)) {y <- factor(y,ordered=TRUE)}
  y2 <- as.numeric(y)
  test <- suppressWarnings(friedman.test(y2,groups,blocks,...))
  test$data.name <- paste0(yname," by ",gname,", block = ",bname)
  test
}

friedman.rating.test.formula <- function(formula,data,subset,na.action,...) {
  if (missing(formula)) {stop("formula missing")}
  if ((length(formula)!=3L) || (length(formula[[3L]])!=3L) ||
    (formula[[3L]][[1L]]!=as.name("|")) || (length(formula[[3L]][[2L]])!=1L) ||
    (length(formula[[3L]][[3L]])!=1L))  {stop("incorrect specification for 'formula'")}
  formula[[3L]][[1L]] <- as.name("+")
  m <- match.call(expand.dots=FALSE)
  m$formula <- formula
  if (is.matrix(eval(m$data, parent.frame()))) {m$data <- as.data.frame(data)}
  m[[1L]] <- as.name("model.frame")
  mf <- eval(m,parent.frame())
  DNAME <- paste0(colnames(mf)[1]," by ",colnames(mf)[2],", block = ",colnames(mf)[3])
  names(mf) <- NULL
  y <- do.call("friedman.rating.test", as.list(mf))
  y$data.name <- DNAME
  y
}
