inv.lsmeans <- function(model,lsm) {
  if ("mer" %in% class(model)) {
    stop(paste("for mixed models please update 'lmer' to version > 1.0 (actual: ",
	packageVersion("lme4"),")",sep=""))
  }
  link <- family(model)$link
  inv.link <- if (link=="identity") {
    expression(x)
  } else if (link=="log") {
    expression(exp(x))
  } else if (link=="inverse") {
    expression(-1/x)
  } else if (link=="logit") {
    expression(1-1/(1+exp(x)))
  } else if (link=="probit") {
    expression(pnorm(x))
  } else if (link=="cloglog") {
    expression(-exp(-exp(x))+1)
  } else if (link=="sqrt") {
    expression(x^2)
  } else {
    stop("link function not recognized")
  }
  ncol.lsm <- grep("lsmean",colnames(lsm[[1]]))
  lev <- interaction(lsm[[1]][,1:(ncol.lsm-1)],sep=":")
  x <- lsm[[1]][,"lsmean"]
  SE <- lsm[[1]][,"SE"]
  d <- D(inv.link,"x")
  means <- eval(inv.link)
  std.err <- abs(eval(d))*SE
  names(means) <- names(std.err) <- lev
  tab <- data.frame(means=means,SE=std.err,row.names=lev)
  means2 <- if(ncol.lsm==2) {
    means
  } else {
    matrix(means,ncol=nlevels(lsm[[1]][,ncol.lsm-1]),dimnames=list(
	levels(interaction(lsm[[1]][,1:(ncol.lsm-2)],sep=":")),
	levels(lsm[[1]][,ncol.lsm-1])))
  }
  std.err2 <- if(ncol.lsm==2) {
    std.err
  } else {
    matrix(std.err,ncol=nlevels(lsm[[1]][,ncol.lsm-1]),dimnames=list(
	levels(interaction(lsm[[1]][,1:(ncol.lsm-2)],sep=":")),
	levels(lsm[[1]][,ncol.lsm-1])))
  }
  res <- list(means=means2,SE=std.err2,tab=tab)
  return(res)
}
