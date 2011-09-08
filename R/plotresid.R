plotresid <-
function(model) {
  if ("lm"%in%class(model)) {
    if (!"glm"%in%class(model)) {
	model.residuals<-residuals(model)
	res.lab<-"Residuals"
    } else {
      if ("negbin"%in%class(model)) {
	  model.residuals<-qresiduals(model)
	  res.lab<-"Quantile residuals"
      } else {
	  laws<-c("poisson","quasipoisson","binomial","quasibinomial")
	  if (model$family[1]%in%laws) {
	    model.residuals<-qresiduals(model)
	    res.lab<-"Quantile residuals"
	  } else {
	    model.residuals<-residuals(model)
	    res.lab<-"Residuals"
	  }
	}
    }
  } else if (class(model)[1]=="mer") {
    model.residuals<-residuals(model)
    res.lab<-"Residuals"
  } else {
    stop("model not recognized")
  }
  fit<-fitted(model)
  cat("Open a new window (y/n)? ")
  resp<-as.character(readLines(n=1))
  if (resp=="y") {
    dev.new(height=6,width=12)
  }
  par(mfrow=c(1,2))
  plot(fit,model.residuals,xlab="Fitted values",ylab=res.lab,main=paste(res.lab,"vs fitted"))
  abline(h=0,col="grey",lty=3)
  panel.smooth(fit,model.residuals)
  qqnorm(model.residuals)
  shapiro.test(model.residuals)
}

