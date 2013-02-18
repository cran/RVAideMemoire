least.rect <-
function(formula,data,conf.level=0.95,theo=1){
  if (missing(formula)||(length(formula)!=3)) {stop("missing or incorrect formula")}
  m <- match.call()
  if (is.matrix(eval(m$data,parent.frame()))) {m$data <- as.data.frame(m$data)}
  m[[1]] <- as.name("model.frame")
  m$conf.level <- m$theo <- NULL
  mf <- eval(m,parent.frame())
  dname <- names(mf)
  x <- mf[,2]
  y <- mf[,1]
  nul <- as.numeric(row.names(table(c(which(is.na(x)),which(is.na(y))))))
  x.2 <- if(length(nul)>0) {x[-nul]} else {x}
  y.2 <- if(length(nul)>0) {y[-nul]} else {y}
  corr <- cor.test(x.2,y.2,method="pearson",conf.level=conf.level)
  r <- as.numeric(corr$estimate)
  k <- qt((1+conf.level)/2,length(x.2)-2)^2*(1-r^2)/(length(x.2)-2)
  b <- sd(y.2)/sd(x.2)*sign(cov(x.2,y.2))
  b.ci1 <- b*sqrt(1+2*k-sqrt((1+2*k)^2-1))
  b.ci2 <- b*sqrt(1+2*k+sqrt((1+2*k)^2-1))
  b.inf <- min(b.ci1,b.ci2)
  b.sup <- max(b.ci1,b.ci2)
  a <- mean(y.2)-b*mean(x.2)
  a.inf <- mean(y.2)-b.sup*mean(x.2)
  a.sup <- mean(y.2)-b.inf*mean(x.2)
  t.obs <- abs(b^2-theo^2)*sqrt(length(x.2)-2)/(2*b*theo*sqrt(1-r^2))
  p <- min(pt(t.obs,length(x.2)-2),pt(t.obs,length(x.2)-2,lower.tail=FALSE))*2
  conf.int <- matrix(c(a.inf,b.inf,a,b,a.sup,b.sup),nrow=2,dimnames=list(c("(Intercept)",dname[2]),
    c("inf","coeff","sup")))
  conform <- data.frame("observed"=b,"theoretical"=theo,"Df"=length(x.2)-2,"t"=t.obs,"Pr(>|t|)"=p,
    " "=.psignif(p),stringsAsFactors=FALSE,check.names=FALSE)
  p.corr <- as.numeric(corr$p.value)
  corr.tab <- data.frame("inf"=as.numeric(corr$conf.int[1]),"r"=r,"sup"=as.numeric(corr$conf.int[2]),
    "Df"=as.numeric(corr$parameter),"t"=as.numeric(corr$statistic),"Pr(>|t|)"=p.corr," "=.psignif(p.corr),
    stringsAsFactors=FALSE,check.names=FALSE)
  coeffs <- c(a,b)
  names(coeffs) <- c("(Intercept)",dname[2])
  fit <- a+b*x
  names(fit) <- 1:length(x)
  res <- y-fit
  names(res) <- 1:length(x)
  result <- list(coefficients=coeffs,x=dname[2],y=dname[1],residuals=res,fitted.values=fit,
    conf.level=conf.level,conf.int=conf.int,slope.theo=theo,df.comp=length(x.2)-2,statistic.comp=t.obs,p.value.comp=p,
    model=data.frame(y,x),comp=conform,r=c("inf"=as.numeric(corr$conf.int[1]),"r"=r,
    "sup"=as.numeric(corr$conf.int[2])),r.df=as.numeric(corr$parameter),r.statistic=as.numeric(corr$statistic),
    r.p.value=p.corr,corr=corr.tab)
  class(result) <- "least.rect"
  return(result)
}

print.least.rect <-
function(x,...) {
  cat("\n        Least rectangles simple linear regression\n\n")
  cat(paste("Equation :",x$y,"=",round(x$conf.int[1,2],5),"+",round(x$conf.int[2,2],5),x$x,"\n\n"))
  cat(100*x$conf.level,"% confidence interval\n",sep="")
  print(x$conf.int,digits=5)
  cat("\nEquality of the slope to",x$slope.theo,"\n")
  print(x$comp,digits=5,row.names=FALSE)
  cat("\nPearson's linear correlation coefficient\n(",100*x$conf.level,"% confidence interval)\n",sep="")
  print(x$corr,digits=5,row.names=FALSE)
  cat("\n")
}

