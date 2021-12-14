back.emmeans <- function(emm,transform=c("log","logit","sqrt","4rt","inverse","p.beta"),base=exp(1),
  add=0,n=NULL,C=2,ord=FALSE,decreasing=TRUE) {
  transform <- match.arg(transform)
  if ("list" %in% class(summary(emm))) {
    emm2 <- summary(emm)$emmeans
  } else {
    emm2 <- summary(emm)
  }
  col <- which(colnames(emm2)=="emmean")
  f <- if (col>2) {
    apply(emm2[,1:(col-1)],1,function(x) paste(x,collapse=":"))
  } else {
    emm2[,1]
  }
  moy <- emm2$emmean
  es <- emm2$SE
  res <- data.frame(a=f,SE.inf=moy-es,EMMean=moy,SE.sup=moy+es)
  colnames(res)[1] <- paste(colnames(emm[1:(col-1)]),collapse=":")
  if (transform=="log") {
    res[,-1] <- base^res[,-1]-add
  } else if (transform=="logit") {
    res[,-1] <- exp(res[,-1])/(1+exp(res[,-1]))-add
  } else if (transform=="sqrt") {
    res[,-1] <- res[,-1]^2-add
  } else if (transform=="4rt") {
    res[,-1] <- res[,-1]^4-add
  } else if (transform=="inverse") {
    res[,-1] <- 1/res[,-1]-add
  } else if (transform=="p.beta") {
    if (!"tran" %in% names(emm@misc)) {warning("are you sure you used mode='link' in emmeans()?")}
    if (is.null(n)) {stop("please provide n, the total number of observations")}
    res[,-1] <- exp(res[,-1])/(1+exp(res[,-1]))
    res[,-1] <- (res[,-1]*n-(1/C))/(n-1)
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
