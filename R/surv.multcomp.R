surv.multcomp <-
function(formula,mat,data,strata=NULL,type=c("coxph","survreg"),dist="exponential",p.method="fdr") {
  if (missing(formula)||(length(formula)!=3)) {stop("missing or incorrect formula")}
  suppressWarnings(if (!type%in%c("coxph","survreg")) {stop("model type not recognized")})
  allnames <- all.names(formula)
  m <- match.call()
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  m$mat <- m$strata <- m$type <- m$dist <- m$p.method <- NULL
  mf <- eval(m,parent.frame())
  if (!is.Surv(mf[,1])) {stop(paste("'",names(mf)[1],"' is not a Surv() object",sep=""))}
  surv <- mf[,1]
  fact <- mf[,2]
  variables <- names(mf)
  if (!is.data.frame(mat)) {
    if (is.matrix(mat)) {
	mat <- as.data.frame(mat)
    } else {stop("'mat' is not a \"data.frame\" object")}
  }
  mat <- t(mat)
  levels(fact) <- abbreviate(levels(fact),3)
  if (nrow(mat)!=nlevels(fact)) {stop("incorrect 'mat' dimensions (number of rows != from number of levels of '",
    names(mf)[2],"')")}
  rownames(mat) <- levels(fact)
  if (length(type)!=1) {type <- "coxph"}
  liste.surv <- list()
  if (!is.null(strata)) {liste.stra <- list()}
  for (i in 1:nlevels(fact)) {
    liste.surv[[i]] <- subset(surv,as.numeric(fact)==i)
    if (!is.null(strata)) {liste.stra[[i]] <- subset(strata,as.numeric(fact)==i)}
  }
  names(liste.surv) <- levels(fact)
  if (!is.null(strata)) {names(liste.stra) <- levels(fact)}
  comparisons <- character(ncol(mat))
  test <- integer(ncol(mat))
  p <- integer(ncol(mat))
  for (i in 1:ncol(mat)) {
    contrast <- mat[,i]
    num1 <- which(contrast>0)
    names(num1) <- rownames(mat)[num1]
    num2 <- which(contrast<0)
    names(num2) <- rownames(mat)[num2]
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
    datas <- data.frame(fac=c(rep("Level1",nrow(ech1)),rep("Level2",nrow(ech2))),rbind(ech1,ech2),stra=strata2)
    if (ncol(datas)==4) {
	surv2 <- Surv(datas$time,datas$status)
    } else if (ncol(datas)==5) {
	surv2 <- Surv(datas$start,datas$stop,datas$status)
    }
    method <- NULL
    model <- if (!is.null(strata)) {
	if (type=="survreg") {
	  method <- "log-rank tests"
	  survreg(surv2~fac+strata(stra),dist=dist,data=datas)
	} else {
	  method <- "likelihood ratio tests"
	  coxph(surv2~fac+strata(stra),data=datas)
	}
    } else {
	if (type=="survreg") {
	  method <- "log-rank tests"
	  survreg(surv2~fac,dist=dist,data=datas)
	} else {
	  method <- "likelihood ratio tests"
	  coxph(surv2~fac,data=datas)
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
    comparisons[i] <- paste(paste(rownames(mat)[which(contrast>0)],collapse="-"),"vs",
	paste(rownames(mat)[which(contrast<0)],collapse="-"))
  }
  p.adj <- p.adjust(p,method=p.method)
  comp <- data.frame(" "=comparisons,"Chi"=test,"Pr(>Chi)"=p.adj," "=.psignif(p.adj),
    stringsAsFactors=FALSE,check.names=FALSE)
  model.txt <- if (!is.null(strata)) {
    if (type=="survreg") {
	paste("survreg(",variables[1]," ~ ",variables[2]," + strata(",
	  deparse(substitute(strata)),"), dist=\"",dist,"\")",sep="")
    } else {
	paste("coxph(",variables[1]," ~ ",variables[2]," + strata(",
	  deparse(substitute(strata)),"))",sep="")
    }
  } else {
    if (type=="survreg") {
	paste("survreg(",variables[1]," ~ ",variables[2],", dist=\"",dist,
	  "\")",sep="")
    } else {
	paste("coxph(",variables[1]," ~ ",variables[2],")",sep="")
    }
  }
  result <- list(model=model.txt,statistic=test,method=method,p.adjust.method=p.method,p.value2=p.adj,p.value=comp)
  class(result) <- "RV.multcomp"
  return(result)
}
