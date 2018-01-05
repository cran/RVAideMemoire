back.emmeans <- function(emm,transform=c("log","logit","sqrt","4rt","inverse"),base=exp(1),
  add=0,ord=FALSE,decreasing=TRUE) {
  transform <- match.arg(transform)
  if ("list" %in% class(summary(emm))) {
    emm <- summary(emm)$emmeans
  } else {
    emm <- summary(emm)
  }
  col <- which(colnames(emm)=="emmean")
  f <- if (col>2) {
    apply(emm[,1:(col-1)],1,function(x) paste(x,collapse=":"))
  } else {
    emm[,1]
  }
  moy <- emm$emmean
  es <- emm$SE
  res <- data.frame(a=f,SE.inf=moy-es,EMMean=moy,SE.sup=moy+es)
  colnames(res)[1] <- paste(colnames(emm[1:(col-1)]),collapse=":")
  res[,-1] <- if (transform=="log") {
    base^res[,-1]-add
  } else if (transform=="logit") {
    exp(res[,-1])/(1+exp(res[,-1]))-add
  } else if (transform=="sqrt") {
    res[,-1]^2-add
  } else if (transform=="4rt") {
    res[,-1]^4-add
  } else if (transform=="inverse") {
    1/res[,-1]-add
  } else {
    stop("unknown transformation")
  }
  if (transform=="inverse") {
    SE1 <- res$SE.inf
    SE2 <- res$SE.sup
    res$SE.inf <- SE2
    res$SE.sup <- SE1
  }
  if (ord) {res <- res[order(res$EMMean,decreasing=decreasing),]}
  return(res)
}
