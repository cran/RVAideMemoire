surv.multcomp <-
function(formula,mat,data=NULL,strata=NULL,type=c("coxph","survreg"),distribution="exponential",
 p.method="fdr") {
  if (all.names(formula)[1]!="~") {stop("incorrect 'formula'")}
  if (length(all.names(formula))==3) {
    variables <- all.vars(formula)
    surv.temp <- if (is.null(data)) {get(variables[1],pos=environment(formula))}
	else {get(variables[1],pos=get(deparse(substitute(data))))}
    if (!is.Surv(surv.temp)) {
	stop(paste("'",variables[1],"' is not a Surv() object",sep=""))
	  } else {
	surv <- surv.temp
	fact <- if (is.null(data)) {get(variables[2],pos=environment(formula))}
	  else {get(variables[2],pos=get(deparse(substitute(data))))}
    }
  } else if (length(all.names(formula))==4) {
    variables <- c(paste("Surv(",all.vars(formula)[1],")",sep=""),all.vars(formula)[2])
    surv.temp <- if (is.null(data)) {get(all.vars(formula)[1],pos=environment(formula))}
	else {get(all.vars(formula)[1],pos=get(deparse(substitute(data))))}
    surv <- Surv(surv.temp)
    fact <- if (is.null(data)) {get(all.vars(formula)[2],pos=environment(formula))}
	else {get(all.vars(formula)[2],pos=get(deparse(substitute(data))))}	
  }  else if (length(all.names(formula))==5) {
    variables.temp <- all.vars(formula)
    variables <- c(paste("Surv(",variables.temp[1],",",variables.temp[2],")",sep=""),variables.temp[3])
    surv.temp <- if (is.null(data)) {get(variables.temp[1],pos=environment(formula))}
	else {get(variables.temp[1],pos=get(deparse(substitute(data))))}
    status <- if (is.null(data)) {get(variables.temp[2],pos=environment(formula))}
	else {get(variables.temp[2],pos=get(deparse(substitute(data))))}
    surv <- Surv(surv.temp,status)
    fact <- if (is.null(data)) {get(variables.temp[3],pos=environment(formula))}
	else {get(variables.temp[3],pos=get(deparse(substitute(data))))}	
  }
  if (nrow(rbind(surv))!=length(fact)) {stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))}
  if (!is.matrix(mat)) {stop("'mat' is not a \"matrix\" object")}
  if (!is.factor(fact)) {
    fact2 <- as.factor(fact)
  } else {
    fact2 <- fact
  }
  if (ncol(mat)!=nlevels(fact2)) {stop("incorrect 'mat' dimensions")}
  colnames(mat) <- levels(fact2)
  suppressWarnings(if (!type%in%c("coxph","survreg")) {stop("model type not recognized")})
  if (length(type)>1) {type <- "coxph"}
  liste.surv <- list()
  if (!is.null(strata)) {liste.stra <- list()}
  for (i in 1:nlevels(fact2)) {
    liste.surv[[i]] <- subset(surv,fact2==levels(fact2)[i])
    if (!is.null(strata)) {liste.stra[[i]] <- subset(strata,fact2==levels(fact2)[i])}
  }
  names(liste.surv) <- levels(fact2)
  if (!is.null(strata)) {names(liste.stra) <- levels(fact2)}
  comparisons <- character(nrow(mat))
  test <- integer(nrow(mat))
  p <- integer(nrow(mat))
  for (i in 1:nrow(mat)) {
    contrast <- mat[i,]
    num1 <- which(contrast>0)
    num2 <- which(contrast<0)
    ech1 <- NULL
    ech2 <- NULL
    if (!is.null(strata)) {
	stra1 <- NULL
	stra2 <- NULL
    }
    for (j in 1:length(liste.surv)) {
	if (any(names(num1)==names(liste.surv)[j])) {
	  ech1 <- rbind(ech1,liste.surv[[j]])
	  if (!is.null(strata)) {stra1 <- c(stra1,liste.stra[[j]])}
	}
	if (any(names(num2)==names(liste.surv)[j])) {
	  ech2 <- rbind(ech2,liste.surv[[j]])
	  if (!is.null(strata)) {stra2 <- c(stra2,liste.stra[[j]])}
	}
    }
    strata2 <- if (!is.null(strata)) {
	c(stra1,stra2)
    } else {
	rep(NA,length(ech1)+length(ech2))
    }
    datas <- data.frame(fac=c(rep("Level1",dim(ech1)[1]),rep("Level2",dim(ech2)[1])),rbind(ech1,ech2),stra=strata2)
    if (ncol(datas)==4) {
	surv2 <- Surv(datas$time,datas$status)
    } else if (ncol(datas)==5) {
	surv2 <- Surv(datas$start,datas$stop,datas$status)
    }
    method <- NULL
    model <- if (!is.null(strata)) {
	if (type=="survreg") {
	  method <- "log-rank test"
	  survreg(surv2~datas$fac+strata(datas$stra),dist=distribution)
	} else {
	  method <- "likelihood ratio test"
	  coxph(surv2~datas$fac+strata(datas$stra))
	}
    } else {
	if (type=="survreg") {
	  method <- "log-rank test"
	  survreg(surv2~datas$fac,dist=distribution)
	} else {
	  method <- "likelihood ratio test"
	  coxph(surv2~datas$fac)
	}
    }
    test[i] <- if (type=="survreg") {
	summary(model)$chi
    } else {
	as.numeric(summary(model)$logtest[1])
    }
    p[i] <- if (type=="survreg") {
	pchisq(test[i],1,lower.tail=FALSE)
    } else {
	as.numeric(summary(model)$logtest[3])
    }
    comparisons[i] <- paste(paste(colnames(mat)[which(contrast>0)],collapse="-"),"vs",
	paste(colnames(mat)[which(contrast<0)],collapse="-"))
  }
  p.adj <- p.adjust(p,method=p.method)
  comp <- data.frame("Chi"=test,"Pr(>Chi)"=p.adj," "=psignif(p.adj),row.names=comparisons,
    stringsAsFactors=FALSE,check.names=FALSE)
  model.txt <- if (!is.null(strata)) {
    if (type=="survreg") {
	paste("model: survreg(",variables[1]," ~ ",variables[2]," + strata(",
	  deparse(substitute(strata)),"), dist=\"",distribution,"\")",sep="")
    } else {
	paste("model: coxph(",variables[1]," ~ ",variables[2]," + strata(",
	  deparse(substitute(strata)),"))",sep="")
    }
  } else {
    if (type=="survreg") {
	paste("model: survreg(",variables[1]," ~ ",variables[2],", dist=\"",distribution,
	  "\")",sep="")
    } else {
	paste("model: coxph(",variables[1]," ~ ",variables[2],")",sep="")
    }  }
  result <- list(model=model.txt,statistic=test,method=method,p.adjust.method=p.method,p.value=p.adj,comp=comp)
  class(result) <- c("surv.multcomp","list")
  return(result)
}

