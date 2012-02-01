perm.anova.2wayD <-
function(resp,fact1,fact2,variables,nperm) {
  if (any(diff(tapply(resp,fact2,length))!=0)) {stop("this function is not made for unbalanced design")}
  if (tapply(resp,list(fact2,fact1),length)[1,1]==1) {stop("no repetition of ",variables[2],":",variables[3]," -> no interaction")}
  anova.ref <- anova(lm(resp~fact1*fact2))
  anova.ref[1,"F value"] <- anova.ref[1,"Mean Sq"]/anova.ref[3,"Mean Sq"]
  F1.ref <- anova.ref[1,"F value"]
  tab <- data.frame("Sum Sq"=round(anova.ref[c(1,4),"Sum Sq"],2),"Df"=anova.ref[c(1,4),"Df"],"Mean Sq"=round(anova.ref[c(1,4),"Mean Sq"],2),
    "F value"=c(round(anova.ref[1,"F value"],4)," "),"Pr(>F)"=NA," "=character(2),stringsAsFactors=FALSE,check.names=FALSE)
  rownames(tab) <- c(variables[2],"Residuals")
  F1.perm <- numeric(nperm+1)
  F1.perm[1] <- F1.ref
  for (i in 1:nperm) {
    anova.perm <- anova(lm(sample(resp)~fact1*fact2))
    F1.perm[i+1] <- anova.perm[1,"Mean Sq"]/anova.perm[3,"Mean Sq"]
   }
  pvalue <- length(which(F1.perm >= F1.ref))/(nperm+1)
  tab[1,"Pr(>F)"] <- format(pvalue,digits=5,nsmall=5)
  return(list(tab=tab))
}
