adjust.esticon <-
function(model,mat,p.method="fdr",...) {
  tab <- esticon(model,mat,...)
  p.adj <- p.adjust(tab[,grep("Pr",colnames(tab))],method=p.method)
  tab2 <- transform(tab," "=character(nrow(tab)),stringsAsFactors=FALSE,check.names=FALSE)
  tab2[,grep("Pr",colnames(tab))] <- p.adj
  tab2[,ncol(tab2)] <- psignif(p.adj)
  if (is.null(colnames(mat))) {colnames(mat) <- LETTERS[1:ncol(mat)]}
  for (i in 1:nrow(mat)) {
    lev1 <- colnames(mat)[which(mat[i,]>0)]
    lev2 <- colnames(mat)[which(mat[i,]<0)]
    row.names(tab2)[i] <- paste(paste(lev1,collapse="-"),paste(lev2,collapse="-"),sep=" vs ")
  }
  result <- list(tab.no.adjust=tab,p.adjust.method=p.method,tab.adjust=tab2)
  class(result) <- c("adjust.esticon","list")
  return(result)
}

