pairwise.G.test <-
function(x,p.method="fdr") {
  fun.p <- function(i,j) {
    y <- x[c(i,j),]
    G.test(y)$p.value
  }
  level.names <- if (!is.null(rownames(x))) {
    rownames(x)
  } else {
    LETTERS[1:nrow(x)]
  }
  tab.p <- pairwise.table(fun.p,level.names,p.adjust.method=p.method)
  result <- list(p.adjust.method=p.method,p.value=tab.p)
  class(result) <- c("pairwise.G.test","list")
  return(result)
}
