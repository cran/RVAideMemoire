val.resid <-
function(model,reg=FALSE,fact=NULL) {
  disc<-c("poisson","binomial","quasipoisson","quasibinomial")
  residus<-NULL
  resid.lab<-""
  if (length(which(disc==model$family[1]))>0) {
    residus<-qresiduals(model)
    resid.lab<-"Quantile residuals"
  } else {
    residus<-residuals(model)
    resid.lab<-"Residuals"
  }
  dev.new(width=11,height=6)
  par(mfrow=c(1,2))
  if (reg==TRUE) {
    plot(model$fitted.values,residus,xlab="Predicted values",ylab=resid.lab,main=paste(resid.lab,"vs Fitted"))
    abline(h=0,lty=2,col="grey")
  } else {
    f<-model$model[,fact]
    boxplot(residus~f,labels=levels(f),xlab="Factor levels",ylab=resid.lab,main=paste("Variance of ",resid.lab))
  }
  qqnorm(residus,xlab="Theoretical Quantiles",ylab=resid.lab,main="Normal Q-Q")
  qqline(residus,lty=2,col="grey")
}