fisher.medtest <-
function(x,y) {
  x2=na.omit(x)
  y2=na.omit(y)
  med<-median(c(x2,y2))
  x.inf<-length(x2[x2<=med])
  x.sup<-length(x2[x2>med])
  y.inf<-length(y2[y2<=med])
  y.sup<-length(y2[y2>med])
  tab<-matrix(c(x.sup,y.sup,x.inf,y.inf),ncol=2,dimnames=list(c("x","y"),c("sup","inf")))
  test<-fisher.test(tab)
  result<-list(datas=c(deparse(substitute(x)),deparse(substitute(y))),median=med,tab.cont=tab,
    odds.ratio=as.numeric(test$estimate),p.value=as.numeric(test$p.value))
  class(result)<-c("fisher.medtest","list")
  return(result)
}

