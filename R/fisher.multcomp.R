fisher.multcomp <-
function(tab.cont,p.method="fdr") {
  if (is.matrix(tab.cont)) {tab.cont <- as.table(tab.cont)}
  if (!is.table(tab.cont)) {stop("'tab.cont' is not a \"table\" object")}
  colonnes <- combn(colnames(tab.cont),2)
  lignes <- combn(rownames(tab.cont),2)
  colonnes2 <- apply(colonnes,2,function(x) paste(x,collapse=":"))
  lignes2 <- apply(lignes,2,function(x) paste(x,collapse=":"))
  p.no.adjust <- matrix(0,nrow=ncol(lignes),ncol=ncol(colonnes),dimnames=list(lignes2,colonnes2))
  for (i in 1:ncol(colonnes)) {
    for (j in 1:ncol(lignes)) {
	tab <- tab.cont[lignes[,j],colonnes[,i]]
	p.no.adjust[j,i] <- fisher.test(tab)$p.value
    }
  }
  p.adjust <- matrix(p.adjust(p.no.adjust,method=p.method),nrow=nrow(p.no.adjust),ncol=ncol(p.no.adjust),
    dimnames=dimnames(p.no.adjust))
  result<-list(p.adjust.method=p.method,p.value=p.adjust)
  class(result)<-c("fisher.multcomp","list")
  return(result)
}

