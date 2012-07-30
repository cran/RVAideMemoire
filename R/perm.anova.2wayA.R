perm.anova.2wayA <-
function(resp,fact1,fact2,variables,nperm) {
  if (any(diff(tapply(resp,list(fact2,fact1),length))!=0)) {stop("this function is not made for unbalanced design")}
  anova.ref <- anova(lm(resp~fact1+fact2))
  F1.ref <- anova.ref[1,"F value"]
  F2.ref <- anova.ref[2,"F value"]
  tab <- data.frame("Sum Sq"=round(anova.ref[,"Sum Sq"],2),"Df"=anova.ref[,"Df"],"Mean Sq"=round(anova.ref[,"Mean Sq"],2),
    "F value"=c(round(anova.ref[1:2,"F value"],4)," "),"Pr(>F)"=NA," "=character(3),stringsAsFactors=FALSE,check.names=FALSE)
  rownames(tab) <- c(variables[2],variables[3],"Residuals")
  F1.perm <- numeric(nperm+1)
  F2.perm <- numeric(nperm+1)
  F1.perm[1] <- F1.ref
  F2.perm[1] <- F2.ref
  pb <- txtProgressBar(min=0,max=100,initial=0,style=3)
  for (i in 1:nperm) {
    anova.perm <- anova(lm(sample(resp)~fact1+fact2))
    F1.perm[i+1] <- anova.perm[1,"F value"]
    F2.perm[i+1] <- anova.perm[2,"F value"]
    setTxtProgressBar(pb,round(i*100/nperm,0))
  }
  cat("\n")
  pvalue1 <- length(which(F1.perm >= F1.ref))/(nperm+1)
  pvalue2 <- length(which(F2.perm >= F2.ref))/(nperm+1)
  tab[1:2,"Pr(>F)"] <- format(c(pvalue1,pvalue2),digits=5,nsmall=5)
  return(list(tab=tab))
}
