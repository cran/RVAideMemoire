perm.anova.3wayA <-
function(resp,fact1,fact2,fact3,variables,nperm) {
  if (any(diff(table(fact1,fact2,fact3))!=0)) {stop("this function is not made for unbalanced design")}
  anova.ref <- anova(lm(resp~fact1*fact3+fact2*fact3))
  MSres <- sum(anova.ref[4:5,"Sum Sq"])/sum(anova.ref[4:5,"Df"]) 
  F1.ref <- anova.ref[1,"Mean Sq"]/MSres
  F2.ref <- anova.ref[3,"Mean Sq"]/MSres
  tab <- data.frame("Sum Sq"=round(anova.ref[c(1,3,6),"Sum Sq"],3),"Df"=anova.ref[c(1,3,6),"Df"],"Mean Sq"=round(anova.ref[c(1,3,6),"Mean Sq"],3),
    "F value"=c(round(c(F1.ref,F2.ref),4)," "),"Pr(>F)"=NA," "=character(3),stringsAsFactors=FALSE,check.names=FALSE)
  rownames(tab) <- c(variables[2],variables[3],"Residuals")
  F1.perm <- numeric(nperm+1)
  F2.perm <- numeric(nperm+1)
  F1.perm[1] <- F1.ref
  F2.perm[1] <- F2.ref
  for (i in 1:nperm) {
    anova.perm <- anova(lm(sample(resp)~fact1*fact3+fact2*fact3))
    MSres.perm <- sum(anova.perm[4:5,"Sum Sq"])/sum(anova.perm[4:5,"Df"]) 
    F1.perm[i+1] <- anova.perm[1,"Mean Sq"]/MSres.perm
    F2.perm[i+1] <- anova.perm[3,"Mean Sq"]/MSres.perm
   }
  pvalue1 <- length(which(F1.perm >= F1.ref))/(nperm+1)
  pvalue2 <- length(which(F2.perm >= F2.ref))/(nperm+1)
  tab[1:2,"Pr(>F)"] <- format(c(pvalue1,pvalue2),digits=5,nsmall=5)
  return(list(tab=tab))
}
