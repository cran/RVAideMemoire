wilcox.signtest <-
function (x,data=NULL,mu=NULL) {
  if (is(x,"formula")) {
    if (all.names(x)[1]!="~") {
	stop("incorrect formula")
    }
    variables <- all.vars(x)
    data.name <- paste("data: ",variables[1]," ~ ",variables[2],sep="")
    resp <- if (is.null(data)) {
	get(variables[1],pos=environment(x))
    } else {
	get(variables[1],pos=get(deparse(substitute(data))))
    }
    fact <- if (is.null(data)) {
	get(variables[2],pos=environment(x))
    } else {
	get(variables[2],pos=get(deparse(substitute(data))))
    }
    if (length(resp)!=length(fact)) {
	stop(paste("'",variables[1],"' and '",variables[2],"' lengths differ",sep=""))
    }
    method <- "Comparison of 2 medians by Wilcoxon sign test"
    ech1 <- resp[as.numeric(fact)==1]
    ech2 <- resp[as.numeric(fact)==2]
    if (length(ech1)!=length(ech2)) {
	stop(paste("'",levels(fact)[1],"' and '",levels(fact)[2],"' lengths differ",sep=""))
    }
    ech1 <- ech1[complete.cases(ech1,ech2)]
    ech2 <- ech2[complete.cases(ech1,ech2)]
    signs <- ech1-ech2
    p <- binom.test(length(signs[signs>0]),length(signs[signs!=0]),0.5)$p.value
  } else {
    if (is.null(mu)) {
	stop("'mu' is missing")
    } else {
	data.name <- paste("data: ",deparse(substitute(x)),sep="")
	method <- "Comparison of one median to a given value\n  by Wilcoxon sign test"
	x <- na.omit(x)
	signs <- x-mu
	p <- binom.test(length(signs[signs>0]),length(signs[signs!=0]),0.5)$p.value
    }
  }
  result <- list(method=method,data.name=data.name,mu=mu,p.value=p)
  class(result) <- c("wilcox.signtest","list")
  return(result)
}

