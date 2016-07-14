cond.multinom <- function(model) {
  if (!"multinom" %in% class(model)) {stop("model not recognized")}
  if (!"Hessian" %in% names(model)) {
    model <- update(model,.~.,Hess=TRUE,trace=FALSE)
  }
  evd <- eigen(model$Hessian,symmetric=TRUE,only.values=TRUE)$values
  cond <- sqrt(max(evd)/min(evd))
  result <- c("Condition number"=cond,"Log10(condition)"=log10(cond))
  return(result)
}
