G.multcomp <-
function (x,p.method="fdr") {
  x <- sort(x)
  fun.p <- function(i,j) {
    xi <- x[i]
    xj <- x[j]
    G.test(c(xi,xj))$p.value
  }
  tab.p <- pairwise.table(fun.p,as.character(x),p.adjust.method=p.method)
  result <- list(method="G-tests",data.name=quote(x),p.adjust.method=p.method,p.value=tab.p)
  class(result) <- "pairwise.htest"
  return(result)
}
