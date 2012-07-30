perm.anova.3wayB <-
function(resp,fact1,fact2,fact3,variables,nperm) {
  if (any(diff(table(fact1,fact2,fact3))!=0)) {stop("this function is not made for unbalanced design")}
  if (table(fact1,fact2,fact3)[1,1,1]==1) {stop("no repetition of ",variables[2],":",variables[3],":",variables[4]," -> no interaction")}
  anova.ref <- anova(lm(resp~fact1*fact2*fact3))
  MSres <- sum(anova.ref[5:7,"Sum Sq"])/sum(anova.ref[5:7,"Df"]) 
  F1.ref <- anova.ref[1,"Mean Sq"]/MSres
  F2.ref <- anova.ref[2,"Mean Sq"]/MSres
  F1F2.ref <- anova.ref[4,"Mean Sq"]/MSres
  tab <- data.frame("Sum Sq"=round(anova.ref[c(1,2,4,8),"Sum Sq"],3),"Df"=anova.ref[c(1,2,4,8),"Df"],"Mean Sq"=round(anova.ref[c(1,2,4,8),"Mean Sq"],3),
    "F value"=c(round(c(F1.ref,F2.ref,F1F2.ref),4)," "),"Pr(>F)"=NA," "=character(4),stringsAsFactors=FALSE,check.names=FALSE)
  rownames(tab) <- c(variables[2],variables[3],paste(variables[2],":",variables[3],sep=""),"Residuals")
  F1.perm <- numeric(nperm+1)
  F2.perm <- numeric(nperm+1)
  F1F2.perm <- numeric(nperm+1)
  F1.perm[1] <- F1.ref
  F2.perm[1] <- F2.ref
  F1F2.perm[1] <- F1F2.ref
  pb <- txtProgressBar(min=0,max=100,initial=0,style=3)
  for (i in 1:nperm) {
    anova.perm <- anova(lm(sample(resp)~fact1*fact2*fact3))
    MSres.perm <- sum(anova.perm[5:7,"Sum Sq"])/sum(anova.perm[5:7,"Df"]) 
    F1.perm[i+1] <- anova.perm[1,"Mean Sq"]/MSres.perm
    F2.perm[i+1] <- anova.perm[2,"Mean Sq"]/MSres.perm
    F1F2.perm[i+1] <- anova.perm[4,"Mean Sq"]/MSres.perm
    setTxtProgressBar(pb,round(i*100/nperm,0))
  }
  cat("\n")
  pvalue1 <- length(which(F1.perm >= F1.ref))/(nperm+1)
  pvalue2 <- length(which(F2.perm >= F2.ref))/(nperm+1)
  pvalue3 <- length(which(F1F2.perm >= F1F2.ref))/(nperm+1)
  tab[1:3,"Pr(>F)"] <- format(c(pvalue1,pvalue2,pvalue3),digits=5,nsmall=5)
  return(list(tab=tab))
}
