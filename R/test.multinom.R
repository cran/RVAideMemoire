test.multinom <- function(model,variable) {
  if (!"multinom" %in% class(model)) {stop("model not recognized")}
  call <- match.call()
  mod.call <- model$call
  mod.call$trace <- FALSE
  var.name <- deparse(substitute(variable))
  mf <- model.frame(model)
  variable <- mf[,var.name]
  resp <- mf[,1]
  lev <- levels(resp)
  comb <- combn(lev,2)
  unique.lev <- unique(comb[1,])
  n.mod <- as.numeric(relevel(factor(comb[1,]),ref=lev[1]))
  cont <- apply(comb,2,function(x) paste0(x[2],"|",x[1]))
  mod.list <- list()
  mod.list[[1]] <- model
  if (ncol(comb)>1) {
    for (i in 2:max(n.mod)) {
	resp.temp <- relevel(resp,ref=unique.lev[i])
	mf[,1] <- resp.temp
	mod.call$data <- mf
	mod.list[[i]] <- eval(mod.call)
    }
  }
  if (is.numeric(variable)) {
    if (all(variable %in% c(0,1))) {
	variable <- factor(variable)
    } else {
	n <- length(cont)
	res <- data.frame(Coeff=integer(n),SE=integer(n),"Odds.ratio"=integer(n),z=integer(n),
	  "Pr(>|z|)"=integer(n),row.names=cont,check.names=FALSE)
	for (i in 1:n) {
	  mod.temp <- mod.list[[n.mod[i]]]
	  summ <- summary(mod.temp)
	  res[i,"Coeff"] <- if (ncol(comb)==1) {
	    summ$coefficients[var.name]
	  } else {
	    summ$coefficients[comb[2,i],var.name]
	  }
	  res[i,"SE"] <- if (ncol(comb)==1) {
	    summ$standard.errors[var.name]
	  } else {
	    summ$standard.errors[comb[2,i],var.name]
	  }
	  res[i,"Odds.ratio"] <- exp(res[i,"Coeff"])
	  res[i,"z"] <- res[i,"Coeff"]/res[i,"SE"]
	  res[i,"Pr(>|z|)"] <- 2*min(pnorm(res[i,"z"]),pnorm(res[i,"z"],lower.tail=FALSE))
	}
	class(res) <- c("anova","data.frame")
    }
  }
  if (is.logical(variable)) {
    variable <- as.factor(variable)
  }
  if (is.factor(variable)) {
    res <- list()
    if (nlevels(variable)==2) {
	var.name <- paste0(var.name,levels(variable)[2])
	n <- length(cont)
	res.temp <- data.frame(Coeff=integer(n),SE=integer(n),"Odds.ratio"=integer(n),z=integer(n),
	  "Pr(>|z|)"=integer(n),row.names=cont,check.names=FALSE)
	for (i in 1:n) {
	  mod.temp <- mod.list[[n.mod[i]]]
	  summ <- summary(mod.temp)
	  res.temp[i,"Coeff"] <- if (ncol(comb)==1) {
	    summ$coefficients[var.name]    
	  } else {
	    summ$coefficients[comb[2,i],var.name]
	  }
	  res.temp[i,"SE"] <- if (ncol(comb)==1) {
	    summ$standard.errors[var.name]
	  } else {
	    summ$standard.errors[comb[2,i],var.name]
	  }
	  res.temp[i,"Odds.ratio"] <- exp(res.temp[i,"Coeff"])
	  res.temp[i,"z"] <- res.temp[i,"Coeff"]/res.temp[i,"SE"]
	  res.temp[i,"Pr(>|z|)"] <- 2*min(pnorm(res.temp[i,"z"]),pnorm(res.temp[i,"z"],lower.tail=FALSE))
	}
	class(res.temp) <- c("anova","data.frame")
	res[[1]] <- res.temp
	names(res) <- paste0(levels(variable)[2],"|",levels(variable)[1])
    } else {
	lev.var <- levels(variable)
	comb.var <- combn(lev.var,2)
	unique.lev.var <- unique(comb.var[1,])
	n.mod.var <- as.numeric(relevel(factor(comb.var[1,]),ref=lev.var[1]))
	cont.var <- apply(comb.var,2,function(x) paste0(x[2],"|",x[1]))
	mod.list.var <- list()
	mod.list.var[[1]] <- mod.list
	for (i in 2:max(n.mod.var)) {
	  mod.list.var[[i]] <- list()
	  var.temp <- relevel(variable,ref=unique.lev.var[i])
	  mf[,var.name] <- var.temp
	  mod.call$data <- mf
	  for (j in 1:max(n.mod)) {
	    resp.temp <- relevel(resp,ref=unique.lev[j])
	    mf[,1] <- resp.temp
	    mod.call$data <- mf
	    mod.list.var[[i]][[j]] <- eval(mod.call)
	  }
	}
	n <- length(cont)
	for (i in 1:length(n.mod.var)) {
	  var.name.temp <- paste0(var.name,comb.var[2,i])
	  res.temp <- data.frame(Coeff=integer(n),SE=integer(n),"Odds.ratio"=integer(n),z=integer(n),
	    "Pr(>|z|)"=integer(n),row.names=cont,check.names=FALSE)
	  for (j in 1:n) {
	    mod.temp <- mod.list.var[[n.mod.var[i]]][[n.mod[j]]]
	    summ <- summary(mod.temp)
	    res.temp[j,"Coeff"] <- if (ncol(comb)==1) {
		summ$coefficients[var.name.temp]
	    } else {
		summ$coefficients[comb[2,j],var.name.temp]
	    }
	    res.temp[j,"SE"] <- if (ncol(comb)==1) {
		summ$standard.errors[var.name.temp]
	    } else {
		summ$standard.errors[comb[2,j],var.name.temp]
	    }
	    res.temp[j,"Odds.ratio"] <- exp(res.temp[j,"Coeff"])
	    res.temp[j,"z"] <- res.temp[j,"Coeff"]/res.temp[j,"SE"]
	    res.temp[j,"Pr(>|z|)"] <- 2*min(pnorm(res.temp[j,"z"]),pnorm(res.temp[j,"z"],lower.tail=FALSE))
	  }
	  class(res.temp) <- c("anova","data.frame")
	  res[[i]] <- res.temp
	}
	names(res) <- cont.var
    }
  }
  return(res)
}

