# lme4 : getME, isLMM

plotresid <- function(model,shapiro=FALSE) {
  res <- get.res(model)
  model.res <- res$residuals
  res.lab <- res$lab
  res.norm <- res$norm
  model.fit <- get.fit(model)
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  if (!inherits(model,"mlm")) {
    if (!res.norm) {
	plot(model.fit,model.res,xlab="Fitted values",ylab=res.lab,main=paste(res.lab,"vs. fitted"))
	abline(h=0,col="grey",lty=3)
	panel.smooth(model.fit,model.res)
    } else {
	par(mfrow=c(1,2))
	plot(model.fit,model.res,xlab="Fitted values",ylab=res.lab,main=paste(res.lab,"vs. fitted"))
	abline(h=0,col="grey",lty=3)
	panel.smooth(model.fit,model.res)
	qqPlot(model.res,lwd=1,grid=FALSE,xlab="Theoretical quantiles",ylab="Sample quantiles")
	if (shapiro) {shapiro.test(model.res)}
    }
  } else {
    mqqnorm(model.res)
    if (shapiro) {mshapiro.test(model.res)}
  }
}

get.res <- function(x,...) {
  UseMethod("get.res")
}

get.res.default <- function(x,...) {stop("unknown model")}

get.res.lm <- function(x,...) {
  if (inherits(x,"mlm")) {get.res.mlm(x)} else
  if (inherits(x,"glm")) {get.res.glm(x)} else {
    list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
  }
}

get.res.mlm <- function(x,...) {
  list(residuals=resid(x),lab="",norm=TRUE)
}

get.res.glm <- function(x,...) {
  if (x$family$family=="gaussian") {
    if (x$family$link=="identity") {
	list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
    } else {
	list(residuals=rstudent(x),lab="Externally studentized residuals",norm=TRUE)
    }
  } else {
    if (x$family$link=="identity") {
	list(residuals=residuals(x,type="response"),lab="Response residuals",norm=FALSE)
    } else {
	list(residuals=rstudent(x),lab="Externally studentized residuals",norm=FALSE)
    }
  }
}

get.res.mer <- function(x,...) {
  stop(paste("for mixed models please update 'lmer' to version > 1.0 (actual: ",
    packageVersion("lme4"),")",sep=""))
}

get.res.glmmadmb <- function(x,...) {
  if (x$family=="gaussian") {
    if (x$link=="identity") {
	list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
    } else {
	list(residuals=residuals(x,type="pearson"),lab="Pearson residuals",norm=TRUE)
    }
  } else {
    if (x$link=="identity") {
	list(residuals=residuals(x,type="response"),lab="Response residuals",norm=FALSE)
    } else {
	list(residuals=residuals(x,type="pearson"),lab="Pearson residuals",norm=FALSE)
    }
  }
}

get.res.merMod <- function(x,...) {
  if (lme4::isLMM(x)) {
    list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
  } else {
    fam <- family(x)
    if (fam$family=="gaussian") {
	if (fam$link=="identity") {
	  list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
	} else {
	  list(residuals=residuals(x,type="pearson"),lab="Pearson residuals",norm=TRUE)
	}
    } else {
	if (fam$link=="identity") {
	  list(residuals=residuals(x,type="response"),lab="Response residuals",norm=FALSE)
	} else {
	  list(residuals=residuals(x,type="pearson"),lab="Pearson residuals",norm=FALSE)
	}
    }
  }
}

get.res.nls <- get.res.gls <- get.res.lmList <- get.res.nlsList <- 
get.res.lmList4 <- function(x,...) {
  list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
}

get.res.lme <- function(x,...) {
  if (inherits(x,"glmmPQL")) {get.res.glmmPQL(x)} else {
    list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
  }
}

get.res.glmmPQL <- function(x,...) {
  fam <- x$family
  if (fam$family=="gaussian") {
    if (fam$link=="identity") {
	list(residuals=residuals(x,type="response"),lab="Response residuals",norm=TRUE)
    } else {
	list(residuals=residuals(x,type="pearson"),lab="Pearson residuals",norm=TRUE)
    }
  } else {
    if (fam$link=="identity") {
	list(residuals=residuals(x,type="response"),lab="Response residuals",norm=FALSE)
    } else {
	list(residuals=residuals(x,type="pearson"),lab="Pearson residuals",norm=FALSE)
    }
  }
}

get.res.survreg <- function(x,...) {
  list(residuals=residuals(x,type="deviance"),lab="Deviance residuals",norm=FALSE)
}

get.res.least.rect <- function(x,...) {
  list(residuals=residuals(x),lab="Response residuals",norm=TRUE)
}


get.fit <- function(x,...) {
  UseMethod("get.fit")
}

get.fit.default <- function(x,...) {
  fitted(x)
}

get.fit.survreg <- function(x,...) {
  predict(x)
}
