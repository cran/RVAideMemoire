ind.contrib <-
function(model,print.diff=FALSE,graph=TRUE) {
  if ("lm"%in%class(model)) {
    coeffs<-model$coefficients
    coeffs.diff<-lm.influence(model)$coefficients
  } else if ("least.rect"%in%class(model)) {
    coeffs<-model$coefficients
    coeffs.mat<-matrix(ncol=2,nrow=nrow(model$model),dimnames=list(1:nrow(model$model),c("(Intercept)",model$x)))
    for (i in 1:nrow(model$model)) {
	coeffs2<-least.rect(model$model[,"x"][-i],model$model[,"y"][-i])$coefficients
	coeffs.mat[i,1]<-coeffs2[1]
	coeffs.mat[i,2]<-coeffs2[2]
    }
    coeffs.diff<-data.frame(coeffs[1]-coeffs.mat[,1],coeffs[2]-coeffs.mat[,2])
    colnames(coeffs.diff)[1]="(Intercept)"
    colnames(coeffs.diff)[2]=model$x
  } else {
    stop("model not recognized")
  }
  coeffs.prop<-100*coeffs.diff/coeffs
  if (graph==TRUE) {
    y.max<-1.05*max(abs(coeffs.prop))
    plot(coeffs.prop[,1],ylim=c(-y.max,y.max),type="l",xlab="Individual",ylab="Difference in parameters (%)")
    abline(h=0,col="grey",lty=3)
    abline(h=-100,col="grey",lty=3)
    abline(h=100,col="grey",lty=3)
    mtext(c("-100","100"),side=2,line=1,at=c(-100,100),col="grey",cex=0.8)
    for (i in 2:ncol(coeffs.prop)) {
	lines(coeffs.prop[,i],col=i)
    }
    legend(0.7*nrow(model$model),0.95*y.max,colnames(coeffs.prop),col=1:ncol(coeffs.prop),lty=1)
  }
  result<-list(print.diff=print.diff,coefficients=coeffs,coefficients.diff=coeffs.diff,coefficients.prop=coeffs.prop)
  class(result)<-c("ind.contrib","list")
  return(result)
}

