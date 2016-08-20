ord.rw <- function(ord,CA=NULL,rw=NULL) {
  if (!inherits(ord,"dudi")) {ord <- to.dudi(ord)}
  if (!is.null(CA)) {
    CA <- MVA.ident(CA)
    if (!inherits(CA,c("COA.ade4","COA.vegan"))) {stop("'CA' not recognized")}
    if (inherits(CA,"COA.vegan")) {CA <- to.dudi(CA)}
    rw <- CA$lw
  } else if (!is.null(rw)) {
    rw <- rw
  } else {
    rw <- ord$lw
  }
  call <- ord$call
  call$row.w <- rw
  res <- eval(call)
  return(res)
}
