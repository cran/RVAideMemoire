perm.anova.1way <-
function(resp,fact1,variables,nperm) {
  anova.ref <- anova(lm(resp~fact1))
  F.ref <- anova.ref[1,"F value"]
  tab <- data.frame("Sum Sq"=round(anova.ref[,"Sum Sq"],2),"Df"=anova.ref[,"Df"],"Mean Sq"=round(anova.ref[,"Mean Sq"],2),
    "F value"=c(round(anova.ref[1,"F value"],4)," "),"Pr(>F)"=NA," "=character(2),stringsAsFactors=FALSE,check.names=FALSE)
  rownames(tab) <- c(variables[2],"Residuals")
  F.perm <- numeric(nperm+1)
  F.perm[1] <- anova.ref[1,"F value"]
  for (i in 1:nperm) {
    anova.perm <- anova(lm(sample(resp)~fact1))
    F.perm[i+1] <- anova.perm[1,"F value"]
  }
  pvalue <- length(which(F.perm >= F.ref))/(nperm+1)
  tab[1,"Pr(>F)"] <- format(pvalue,digits=5,nsmall=5)
  return(list(tab=tab))
}
