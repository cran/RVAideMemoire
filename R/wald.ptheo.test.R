wald.ptheo.test <-
function(y,blocks=NULL,p=0.5) {
  if (p<=0 | p>=1) {stop("wrong p: 0 < p < 1")}
  call <- match.call()
  cally <- as.character(call$y)
  callb <- as.character(call$blocks)
  namey <- if (length(cally)>1) {
    paste0(cally[2],cally[1],cally[3])
  } else {as.character(cally)}
  nameb <- if (length(callb)>1) {
    paste0(callb[2],callb[1],callb[3])
  } else {as.character(callb)}
  y <- if (is.vector(y)) {
    as.numeric(factor(y))-1
  } else {
    as.matrix(y)
  }
  if (!is.null(blocks)) {
    model <- lme4::glmer(y~1+(1|blocks),family="binomial")
    p.est <- c("probability of success"=unique(predict(model,type="response",re.form=NA)))
    dname <- paste0(namey," and ",nameb)
  } else {
    model <- glm(y~1,family="binomial")
    p.est <- c("probability of success"=unique(predict(model,type="response")))
    dname <- namey
  }
  int.theo <- log(p/(1-p))
  summ <- summary(model)
  int.moy <- summ$coefficients["(Intercept)","Estimate"]
  int.se <- summ$coefficients["(Intercept)","Std. Error"]
  z <- c(z=(int.moy-int.theo)/int.se)
  pval <- min(pnorm(z),pnorm(z,lower.tail=FALSE))*2
  null <- c("probability of success"=p)
  res <- list(statistic=z,p.value=pval,estimate=p.est,null.value=null,
    alternative="two.sided",method="Wald test",data.name=dname)
  class(res) <- "htest"
  return(res)
}
