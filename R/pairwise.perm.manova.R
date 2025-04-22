# vegan : adonis2

pairwise.perm.manova <- function(resp,fact,test=c("Pillai","Wilks","Hotelling-Lawley","Roy","Spherical"),
  nperm=999,progress=TRUE,p.method="fdr",F=FALSE,R2=FALSE) {
  call <- match.call()
  dname <- paste0(deparse(call$resp)," by ",deparse(substitute(fact)),"\n",nperm," permutations")
  fact <- factor(fact)
  if ("dist" %in% class(resp)) {
    fun.p <- function(i,j) {
	fact2 <- droplevels(fact[as.numeric(fact) %in% c(i,j)])
	resp2 <- as.matrix(resp)
	rows <- which(fact %in% levels(fact2))
	resp2 <- as.dist(resp2[rows,rows])
	vegan::adonis2(resp2~fact2,permutations=nperm,by="terms")[1,"Pr(>F)"]
    }
    multcomp.P <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
    if (F) {
	fun.F <- function(i,j) {
	  fact2 <- droplevels(fact[as.numeric(fact) %in% c(i,j)])
	  resp2 <- as.matrix(resp)
	  rows <- which(fact %in% levels(fact2))
	  resp2 <- as.dist(resp2[rows,rows])
	  vegan::adonis2(resp2~fact2,permutations=nperm,by="terms")[1,"F"]
	}
	multcomp.F <- pairwise.table(fun.F,levels(fact),p.adjust.method="none")
    }
    if (R2) {
	fun.R2 <- function(i,j) {
	  fact2 <- droplevels(fact[as.numeric(fact) %in% c(i,j)])
	  resp2 <- as.matrix(resp)
	  rows <- which(fact %in% levels(fact2))
	  resp2 <- as.dist(resp2[rows,rows])
	  vegan::adonis2(resp2~fact2,permutations=nperm,by="terms")[1,"R2"]
	}
	multcomp.R2 <- pairwise.table(fun.R2,levels(fact),p.adjust.method="none")
    }
    method <- "permutation MANOVAs on a distance matrix"
  } else {
    if (nrow(resp)!=length(fact)) {
	stop(paste("'",deparse(substitute(resp)),"' and '",deparse(substitute(fact)),
	  "' lengths differ",sep=""))
    }
    test <- match.arg(test)
    if (!is.matrix(resp)) {resp <- as.matrix(resp)}
    if (!is.factor(fact)) {fact <- factor(fact)}
    fun.p <- function(i,j) {
	resp2 <- resp[as.numeric(fact) %in% c(i,j),]
	fact2 <- droplevels(fact[as.numeric(fact) %in% c(i,j)])
	perm.manova(resp2,fact2,test=test,nperm=nperm,progress)$p
    }
    multcomp.P <- pairwise.table(fun.p,levels(fact),p.adjust.method=p.method)
    if (F) {
	fun.F <- function(i,j) {
	  resp2 <- resp[as.numeric(fact) %in% c(i,j),]
	  fact2 <- droplevels(fact[as.numeric(fact) %in% c(i,j)])
	  perm.manova(resp2,fact2,test=test,nperm=nperm,progress)$F
	}
	multcomp.F <- pairwise.table(fun.F,levels(fact),p.adjust.method="none")
    }
    method <- paste0("permutation MANOVAs (test: ",test,")")
  }
  if ("dist" %in% class(resp)) {
    if (F & R2) {
	result <- list(method=method,data.name=dname,p.value=multcomp.P,p.adjust.method=p.method,F.value=multcomp.F,R2.value=multcomp.R2)
    } else if (F) {
	result <- list(method=method,data.name=dname,p.value=multcomp.P,p.adjust.method=p.method,F.value=multcomp.F)
    } else if (R2) {
	result <- list(method=method,data.name=dname,p.value=multcomp.P,p.adjust.method=p.method,R2.value=multcomp.R2)
    } else {
	result <- list(method=method,data.name=dname,p.value=multcomp.P,p.adjust.method=p.method)
    }
  } else {
    if (F) {
	result <- list(method=method,data.name=dname,p.value=multcomp.P,p.adjust.method=p.method,F.value=multcomp.F)
    } else {
	result <- list(method=method,data.name=dname,p.value=multcomp.P,p.adjust.method=p.method)
    }
  }
  class(result) <- "pairwise.htest"
  return(result)
}

perm.manova <- function(resp,fact,test,nperm,progress) {
  manova.ref <- anova(lm(resp~fact),test=test)
  stat <- ifelse(test!="Spherical","approx F","F")
  F.ref <- manova.ref[2,stat]
  F.perm <- numeric(nperm+1)
  F.perm[1] <- F.ref
  if (progress) {pb <- txtProgressBar(min=0,max=100,initial=0,style=3)}
  for (i in 1:nperm) {
    manova.perm <- anova(lm(resp[sample(1:nrow(resp)),]~fact),test=test)
    F.perm[i+1] <- manova.perm[2,stat]
    if (progress) {setTxtProgressBar(pb,round(i*100/nperm,0))}
  }
  if (progress) {cat("\n")}
  pvalue <- length(which((F.perm+.Machine$double.eps/2) >= F.ref))/(nperm+1)
  return(list(p=pvalue,F=F.ref))
}

