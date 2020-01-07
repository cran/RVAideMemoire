# grDevices: n2mfrow
# car: Anova

multtest.gp.bin <- function(tab,fac,test=c("LRT","Fisher"),p.method="fdr",ordered=TRUE,...) {
  test <- match.arg(test)
  tab <- as.data.frame(tab)
  fac <- droplevels(factor(fac))
  nlev <- nlevels(fac)
  if (nlev<2) {stop("at least 2 groups are needed")}
  gp.prop <- as.matrix(t(aggregate(as.matrix(tab)~fac,FUN=function(x) sum(na.omit(x))/length(na.omit(x))))[-1,])
  colnames(gp.prop) <- paste0("Prop.",abbreviate(levels(fac),1))
  mode(gp.prop) <- "numeric"
  gp.se <- as.matrix(t(aggregate(as.matrix(tab)~fac,FUN=function(x) se(sum(na.omit(x)),length(na.omit(x)))))[-1,])
  colnames(gp.se) <- paste0("SE.",abbreviate(levels(fac),1))
  mode(gp.se) <- "numeric"
  test.f <- switch(test,LRT=multtest.gp.bin.lrt,Fisher=multtest.gp.bin.fisher)
  tab.res <- test.f(tab,fac,...)
  tab.res$P.value <- p.adjust(tab.res$P.value,method=p.method)
  nc <- ncol(tab.res)
  tab.res[,nc+1] <- integer(ncol(tab))
  tab.res <- cbind(tab.res,gp.prop,gp.se)
  tab.res <- signif(tab.res,5)
  tab.res <- as.data.frame(tab.res)
  tab.res[,nc+1] <- .psignif(tab.res$P.value)
  colnames(tab.res)[nc+1] <- " "
  if (ordered) {tab.res <- tab.res[order(tab.res$P.value),]}
  res <- list(tab=tab.res,p.method=p.method,labs=levels(fac))
  class(res) <- c("multtest","multtest.gp.bin","multtest.gp","list")
  return(res)
}

multtest.gp.bin.lrt <- function(tab,fac,...) {
  nvar <- ncol(tab)
  lab <- colnames(tab)
  res <- data.frame(Chisq=integer(nvar),P.value=integer(nvar),row.names=lab)
  for (i in 1:ncol(tab)) {
    x <- tab[,i]
    mod <- glm(x~fac,family="binomial")
    test <- car::Anova(mod,test="LR",...)
    res[i,] <- test[1,c("LR Chisq","Pr(>Chisq)")]
  }
  return(res)
}

multtest.gp.bin.fisher <- function(tab,fac,...) {
  nvar <- ncol(tab)
  lab <- colnames(tab)
  res <- data.frame(P.value=integer(nvar),row.names=lab)
  for (i in 1:ncol(tab)) {
    x <- tab[,i]
    tab.cont <- table(fac,relevel(factor(x),ref="1"))
    test <- fisher.test(tab.cont,...)
    res[i,] <- test$p.value
  }
  return(res)
}

plot.multtest.gp.bin <- function(x,signif=FALSE,alpha=0.05,vars=NULL,xlab="Group",ylab="Mean (+/- SE) proportion",
  titles=NULL,groups=NULL,...) {
  rows <- if (signif) {
    which(x$tab$P.value<=alpha)
  } else {
    1:nrow(x$tab)
  }
  rows <- if (is.null(vars)) {
    1:length(rows)
  } else {
    vars
  }
  tab2 <- x$tab[rows,]
  n <- length(rows)
  nc <- which(colnames(tab2)==" ")
  col.m <- (nc+1):(nc+length(x$labs))
  col.s <- ((nc+1)+length(x$labs)):(nc+2*length(x$labs))
  labs <- if (is.null(groups)) {x$labs} else {groups}
  par(mfrow=grDevices::n2mfrow(n))
  for (i in 1:n) {
    m <- unlist(tab2[i,col.m])
    names(m) <- labs
    s <- unlist(tab2[i,col.s])
    ymin <- ifelse(any(m-s<0),1.3*min(m-s),0)
    ymax <- ifelse(any(m+s>0),1.3*max(m+s),0)
    g <- barplot(m,main=ifelse(is.null(titles),rownames(tab2)[i],titles[i]),xlab=xlab,ylab=ylab,
	ylim=c(ymin,ymax),...)
    arrows(g,m-s,g,m+s,code=3,angle=90,length=0.06)
    ypval <- ifelse(any(m+s>0),1.2*max(m+s),1.2*min(m-s))
    P <- ifelse(tab2$P.value[i]<0.0001,"P < 0.0001",paste("P =",round(tab2$P.value[i],4)))
    text(mean(g),ypval,P)
  }
}


