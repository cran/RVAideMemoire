adjust.esticon <-
function(model,mat,p.method="fdr",...) {
  tab <- esticon(model,mat,...)
  p.adj <- p.adjust(tab[,grep("Pr",colnames(tab))],method=p.method)
  tab2 <- cbind(" "=character(nrow(tab)),tab," "=character(nrow(tab)),stringsAsFactors=FALSE)
  tab2[,grep("Pr",colnames(tab))] <- p.adj
  tab2[,ncol(tab2)] <- .psignif(p.adj)
  if (is.null(colnames(mat))) {colnames(mat) <- LETTERS[1:ncol(mat)]}
  for (i in 1:nrow(mat)) {
    lev1 <- colnames(mat)[which(mat[i,]>0)]
    lev2 <- colnames(mat)[which(mat[i,]<0)]
    tab2[i,1] <- paste(paste(lev1,collapse="-"),paste(lev2,collapse="-"),sep=" vs ")
  }
  result <- list(method="the contrasts method",p.adjust.method=p.method,p.value=tab2)
  class(result) <- "RV.multcomp"
  return(result)
}
