pairwise.wilcox.rating.test <-
function (x,g,p.adjust.method="fdr",
  paired=FALSE,...) {
  p.adjust.method <- match.arg(p.adjust.method)
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
  g <- factor(g)
  METHOD <- if (paired) {
    "Wilcoxon signed rank test"
  } else {
    "Wilcoxon rank sum test"
  }
  compare.levels <- function(i,j) {
    xi <- x[as.integer(g)==i]
    xj <- x[as.integer(g)==j]
    wilcox.rating.test(xi,xj,paired=paired,...)$p.value
  }
  PVAL <- pairwise.table(compare.levels,levels(g),p.adjust.method)
  ans <- list(method=METHOD,data.name=DNAME,p.value=PVAL, 
      p.adjust.method=p.adjust.method)
  class(ans) <- "pairwise.htest"
  ans
}
