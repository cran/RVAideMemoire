# lme4 : getME, isLMM
# statmod : qresiduals

plotresid <-
function (model,shapiro=FALSE) {
  if ("mer" %in% class(model)) {
    stop(paste("for mixed models please update 'lmer' to version > 1.0 (actual: ",
	packageVersion("lme4"),")",sep=""))
  }
  if ("lm"%in%class(model) & !"mlm"%in%class(model)) {
    if (!"glm"%in%class(model)) {
	model.residuals <- rstudent(model)
	res.lab <- "Externally studentized residuals"
    } else {
	if ("negbin"%in%class(model)) {
	  model.residuals <- statmod::qresiduals(model)
	  res.lab <- "Quantile residuals"
	} else {
	  laws <- c("poisson","quasipoisson","binomial","quasibinomial")
	  if (model$family[1]%in%laws) {
	    model.residuals <- statmod::qresiduals(model)
	    res.lab <- "Quantile residuals"
	  } else {
	    model.residuals <- rstudent(model)
	    res.lab <- "Externally studentized residuals"
	  }
	}
    }
  } else if ("glmmadmb"%in%class(model)) {
    model.residuals <- model$resid
    res.lab <- "Residuals"
  } else if(inherits(model,"merMod")) {
    if (lme4::isLMM(model)) {
	model.residuals <- residuals(model)
	res.lab <- "Residuals"
    } else {
	fam <- family(model)$family
	if (fam=="poisson") {
	  y <- lme4::getME(model,"y")
	  mu <- fitted(model)
	  a <- ppois(y-1,mu)
	  b <- ppois(y,mu)
	  u <- runif(n=length(y),min=a,max=b)
	  model.residuals <- qnorm(u)
	  res.lab <- "Quantile residuals"
	} else if (fam=="binomial") {
	  p <- fitted(model)
	  y <- lme4::getME(model,"y")
	  mf <- model.frame(model)
	  if ("(weights)"%in%colnames(mf)) { 
	    n <- mf$weights
	  } else {
	    n <- rep(1,length(y))
	  }
	  y <- n*y
	  a <- pbinom(y-1,n,p)
	  b <- pbinom(y,n,p)
	  u <- runif(n=length(y),min=a,max=b)
	  model.residuals <- qnorm(u)
	  res.lab <- "Quantile residuals"
	} else if (grepl("Negative Binomial",fam)) {
	  y <- lme4::getME(model,"y")
	  size <- model@theta
	  mu <- fitted(model)
	  p <- size/(mu+size)
	  a <- ifelse(y>0,pbeta(p,size,pmax(y,1)),0)
	  b <- pbeta(p,size,y+1)
	  u <- runif(n=length(y),min=a,max=b)
	  model.residuals <- qnorm(u)
	  res.lab <- "Quantile residuals"
	} else {
	  model.residuals <- residuals(model)
	  res.lab <- "Residuals"
	}
    }
  } else if (any(c("lme","nls")%in%class(model))) {
    model.residuals <- resid(model,type="pearson")
    res.lab <- "Standardized residuals"
  } else if ("nlsList"%in%class(model)) {
    model.residuals <- resid(model,type="pooled")
    res.lab <- "Standardized residuals"
  } else if (any(c("survreg","least.rect")%in%class(model))) {
    model.residuals <- residuals(model)
    res.lab <- "Residuals"
  } else if ("mlm"%in%class(model)) {
  } else {
    stop("model not recognized")
  }
  fit <- if (any(c("lm","least.rect","glmmadmb","lme","nls","nlsList")%in%class(model)) | inherits(model,"merMod")) {
    fitted(model)
  } else if ("survreg"%in%class(model)) {
    predict(model)
  }
  opar <- par(no.readonly=TRUE)
  on.exit(par(opar))
  if (!"mlm"%in%class(model)) {
    par(mfrow=c(1,2))
    plot(fit,model.residuals,xlab="Fitted values",ylab=res.lab,main=paste(res.lab,"vs fitted"))
    abline(h=0,col="grey",lty=3)
    panel.smooth(fit,model.residuals)
    qqPlot(model.residuals,lwd=1,grid=FALSE,xlab="Theoretical quantiles",ylab="Sample quantiles")
    if (shapiro) {
	shapiro.test(model.residuals)
    }
  } else {
    mqqnorm(resid(model))
    if (shapiro) {
	mshapiro.test(resid(model))
    }
  }
}

