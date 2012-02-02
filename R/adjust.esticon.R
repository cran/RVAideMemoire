adjust.esticon <-
function(model,matrix,p.method="fdr",...) {
  tab <- esticon(model,matrix,...)
  p.adj <- p.adjust(tab[,grep("Pr",colnames(tab))],method=p.method)
  tab2 <- transform(tab," "=character(nrow(tab)),stringsAsFactors=FALSE,check.names=FALSE)
  tab2[,grep("Pr",colnames(tab))] <- p.adj
  tab2[,ncol(tab2)] <- psignif(p.adj)
  result <- list(tab.no.adjust=tab,p.adjust.method=p.method,tab.adjust=tab2)
  class(result) <- c("adjust.esticon","list")
  return(result)
}

