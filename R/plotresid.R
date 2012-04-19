plotresid <-
function (model,shapiro=FALSE) {
  if ("lm"%in%class(model)) {
    if (!"glm"%in%class(model)) {
	model.residuals <- residuals(model)
	res.lab <- "Residuals"
    } else {
	if ("negbin"%in%class(model)) {
	  model.residuals <- qresiduals(model)
	  res.lab <- "Quantile residuals"
	} else {
	  laws <- c("poisson","quasipoisson","binomial","quasibinomial")
	  if (model$family[1]%in%laws) {
	    model.residuals <- qresiduals(model)
	    res.lab <- "Quantile residuals"
	  } else {
	    model.residuals <- residuals(model)
	    res.lab <- "Residuals"
	  }
	}
    }
  } else if ("mer"%in%class(model)) {
    model.residuals <- model@resid
    res.lab <- "Residuals"
  } else if ("survreg"%in%class(model)) {
    model.residuals <- residuals(model)
    res.lab <- "Residuals"
  } else if ("least.rect"%in%class(model)) {
    model.residuals <- residuals(model)
    res.lab <- "Residuals"
  } else {
    stop("model not recognized")
  }
  fit <- if ("lm"%in%class(model)) {
    fitted(model)
  } else if ("mer"%in%class(model)) {
    model@X%*%fixef(model)
  } else if ("survreg"%in%class(model)) {
    predict(model)
  } else if ("least.rect"%in%class(model)) {
    fitted(model)
  }
  par(mfrow=c(1,2))
  plot(fit,model.residuals,xlab="Fitted values",ylab=res.lab,main=paste(res.lab,"vs fitted"))
  abline(h=0,col="grey",lty=3)
  panel.smooth(fit,model.residuals)
  qqnorm(model.residuals)
  if (shapiro) {
    shapiro.test(model.residuals)
  }
}


