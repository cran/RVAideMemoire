ancova.multcomp <-
function(model) {
  if (ncol(model.frame(model))!=3) {stop("this function is designed only for models containing
   1 covariate and 1 factor")}
  m.frame <- na.omit(as.data.frame(model.frame(model)))
  facteur <- colnames(m.frame)[which(colnames(m.frame)%in%names(model$xlevels))]
  var <- as.numeric(m.frame[,1])
  covar <- as.numeric(m.frame[,1+which(colnames(m.frame)[2:3]!=facteur)])
  fact <- factor(m.frame[,1+which(colnames(m.frame)[2:3]==facteur)])
  covar2 <- covar-mean(covar)
  m <- if ("lm"%in%class(model) & length(class(model))==1) {
    lm(var~covar2+fact)
  } else if ("glm"%in%class(model)) {
    glm(var~covar2+fact,family=family(model))
  } else if ("negbin"%in%class(model)) {
    glm.nb(var~covar2+fact)
  } else {
    stop("model not recognized")
  }
  New <- expand.grid(fact=levels(fact),covar2=0)
  pred <- predict(m,newdata=New,se.fit=TRUE)
  ci <- qnorm(0.025,0,1,lower.tail=FALSE)*pred$se.fit
  tab <- data.frame(lwr=pred$fit-ci,mean=pred$fit,upr=pred$fit+ci,row.names=levels(fact))
  dotchart(pred$fit,xlim=c(0.9*min(pred$fit-ci),1.1*max(pred$fit+ci)),labels=levels(fact),pch=16,
    xlab=colnames(m.frame)[which(colnames(m.frame) != facteur)][2],
    main="Adjusted means +/- 95% confidence interval")
  segments(pred$fit-ci,1:nlevels(fact),pred$fit+ci,1:nlevels(fact))
  segments(pred$fit-ci,1:nlevels(fact)-(nlevels(fact)/40),pred$fit-ci,1:nlevels(fact)+(nlevels(fact)/40))
  segments(pred$fit+ci,1:nlevels(fact)-(nlevels(fact)/40),pred$fit+ci,1:nlevels(fact)+(nlevels(fact)/40))
  print(tab)
}
