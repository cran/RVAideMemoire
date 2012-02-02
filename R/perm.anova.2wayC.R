perm.anova.2wayC <-
function(resp,fact1,fact2,nest.f2,variables,nperm) {
  if (any(diff(tapply(resp,fact2,length))!=0)) {stop("this function is not made for unbalanced design")}
  anova.ref <- anova(lm(resp~fact1/fact2))
  if (nest.f2=="random") {anova.ref[1,"F value"] <- anova.ref[1,"Mean Sq"]/anova.ref[2,"Mean Sq"]}
  F1.ref <- anova.ref[1,"F value"]
  F2.ref <- anova.ref[2,"F value"]
  tab <- data.frame("Sum Sq"=round(anova.ref[,"Sum Sq"],2),"Df"=anova.ref[,"Df"],"Mean Sq"=round(anova.ref[,"Mean Sq"],2),
    "F value"=c(round(anova.ref[1:2,"F value"],4)," "),"Pr(>F)"=NA," "=character(3),stringsAsFactors=FALSE,check.names=FALSE)
  rownames(tab) <- c(variables[2],paste(variables[2],variables[3],sep=":"),"Residuals")
  F1.perm <- numeric(nperm+1)
  F2.perm <- numeric(nperm+1)
  F1.perm[1] <- F1.ref
  F2.perm[1] <- F2.ref
  for (i in 1:nperm) {
    anova.perm1 <- anova(lm(sample(resp)~fact1/fact2))
    if (nest.f2=="random") {
	F1.perm[i+1] <- anova.perm1[1,"Mean Sq"]/anova.perm1[2,"Mean Sq"]
    } else {
	F1.perm[i+1] <- anova.perm1[1,"F value"]
    }
    ordre <- order(fact1)
    repet <- length(resp)/nlevels(fact1)
    ordre.new <- integer(length(ordre))
    for (j in 1:nlevels(fact1)) {ordre.new[(j*repet-(repet-1)):(j*repet)] <- sample(ordre[(j*repet-(repet-1)):(j*repet)],repet)}
    anova.perm2 <- anova(lm(resp[ordre.new]~fact1/fact2))
    F2.perm[i+1] <- anova.perm2[2,"F value"]
  }
  pvalue1 <- length(which(F1.perm >= F1.ref))/(nperm+1)
  pvalue2 <- length(which(F2.perm >= F2.ref))/(nperm+1)
  tab[1:2,"Pr(>F)"] <- format(c(pvalue1,pvalue2),digits=5,nsmall=5)
  if (tapply(resp,fact2,length)[1]==1) {
    warning("only 1 observation per level of '",variables[3],"', permutation of '",variables[2],"' only")
    tab[2,"Pr(>F)"] <- "NA"
  }
  return(list(tab=tab))
}
