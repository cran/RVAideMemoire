multinomial.multcomp <-
function (x,p.method="fdr") {
  fact <- FALSE
  if (is.factor(x)) {
    lab <- levels(x)
    x <- as.vector(table(x))
    names(x) <- lab
    fact <- TRUE
  } else {
    x <- sort(x)
  }
  fun.p <- function(i,j) {
    xi <- x[i]
    xj <- x[j]
    binom.test(xi,sum(xi,xj),p=0.5)$p.value
  }
  tab.p <- pairwise.table(fun.p,as.character(x),p.adjust.method=p.method)
  if (fact) {
    rownames(tab.p) <- lab[-1]
    colnames(tab.p) <- lab[-length(lab)]
  }
  call <- match.call()
  dname.x <- if(length(call$x)==1) {call$x} else {paste(call$x[1],"(",paste(call$x[-1],collapse=","),")",sep="")}
  result <- list(method="exact binomial tests",data.name=dname.x,p.adjust.method=p.method,p.value=tab.p)
  class(result) <- "pairwise.htest"
  return(result)
}
