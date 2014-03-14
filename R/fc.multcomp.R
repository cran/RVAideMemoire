fc.multcomp <- function(model,term=NULL,mat=NULL,p.method="fdr") {
  if (is.null(term)) {stop("no term")}
  termes <- attr(terms(model),"term.label")
  if (!term%in%termes) {stop("unknown term")}
  vars.in.term <- if(grepl(":",term)) {
    strsplit(term,split=":")[[1]]
  } else {term}
  mf <- model.frame(model)
  name.coefs <- NULL
  name.coefs.short <- NULL
  type <- character(length(vars.in.term))
  for (i in 1:length(vars.in.term)) {
    if (is.numeric(mf[,vars.in.term[i]])) {
	name.coefs <- paste(name.coefs,vars.in.term[i],sep="")
	name.coefs.short <- paste(name.coefs.short,vars.in.term[i],sep="")
	type[i] <- "covar"
    }
    if (is.factor(mf[,vars.in.term[i]])) {
	type[i] <- "fact"
	if (is.null(name.coefs)) {
	  name.coefs <- paste(vars.in.term[i],levels(mf[,vars.in.term[i]]),sep="")
	  name.coefs.short <- levels(mf[,vars.in.term[i]])
	} else {
	  temp <- list()
	  temp.short <- list()
	  for (j in 1:length(name.coefs)) {
	    temp[[j]] <- paste(name.coefs[j],vars.in.term[i],levels(mf[,vars.in.term[i]]),sep="")
	    temp.short[[j]] <- paste(name.coefs.short[j],levels(mf[,vars.in.term[i]]),sep="")
	  }
	  name.coefs <- unlist(temp)
	  name.coefs.short <- unlist(temp.short)
	}
    }
    if (i<length(vars.in.term)) {
	name.coefs <- paste(name.coefs,":",sep="")
	name.coefs.short <- paste(name.coefs.short,":",sep="")
    }
  }
  if (all(type=="covar")) {stop("no comparison with covariates only!")}
  if (is.null(mat)) {
    n <- 1:length(name.coefs)
    names(n) <- name.coefs
    mat <- multcomp::contrMat(n,type="Tukey")
  }
  if (!is.matrix(mat)) {mat <- as.matrix(mat)}
  colnames(mat) <- name.coefs
  for (i in 1:nrow(mat)) {
    rownames(mat)[i] <- paste(paste(name.coefs.short[which(mat[i,]>0)],collapse="-"),"vs",
	paste(name.coefs.short[which(mat[i,]<0)],collapse="-"))
  }
  if (all(type=="fact") & sum(type=="fact")>1) {stop("interactions bewteen factors are not handled, see lsmeans() (package 'lsmeans')")}
  if (sum(type=="fact")>1) {stop("interactions between covariate(s) and more than 1 factor are not handled")}
  mod.coefs <- coef(model)
  k.mat <- if (is.vector(mod.coefs)) {
    matrix(0,ncol=length(mod.coefs),nrow=ncol(mat))
  } else if (is.data.frame(mod.coefs)) {
    matrix(0,ncol=ncol(mod.coefs),nrow=ncol(mat))
  } else {
    matrix(0,ncol=ncol(mod.coefs[[1]]),nrow=ncol(mat))
  }
  contra <- attr(model.matrix(model),"contrasts")
  name.mod.coefs <- if (is.vector(mod.coefs)) {
    names(mod.coefs)
  } else if (is.data.frame(mod.coefs)) {
    colnames(mod.coefs)
  } else {
    colnames(mod.coefs[[1]])
  }
  columns <- NULL
  for (i in 1:ncol(mat)) {
    if (colnames(mat)[i]%in%name.mod.coefs) {
	columns <- c(columns,which(name.mod.coefs==colnames(mat)[i]))
    }
  }
  eval(parse(text=paste("k.mat[,columns] <- ",contra,"(ncol(mat))",sep="")))
  comp <- multcomp::glht(model,linfct=mat%*%k.mat)
  summ.comp <- summary(comp,test=multcomp::adjusted(type=p.method))
  pq <- summ.comp$test
  tab <- data.frame(names(pq$coefficients),pq$tstat,pq$pvalues,.psignif(pq$pvalues),
    stringsAsFactors=FALSE,check.names=FALSE)
  rownames(tab) <- NULL
  name.stat <- ifelse(summ.comp$df==0,"z","t")
  colnames(tab) <- c(" ",name.stat,paste("Pr(>|",name.stat,"|)",sep="")," ")
  call <- if(isS4(model)) {
    capture.output(model@call)
  } else {
    capture.output(model$call)
  }
  result <- list(method="general linear hypotheses",model=call,p.value=tab,p.adjust.method=pq$type)
  class(result) <- "RV.multcomp"
  return(result)
}
