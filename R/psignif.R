psignif <-
function(p) {
  result<-character(length(p))
  for (i in 1:length(p)) {
    if (p[i]>0.05) {result[i]<-"NS"} else
    if (p[i]<0.05 & p[i]>0.01) {result[i]<-"*"} else
    if (p[i]<0.01 & p[i]>0.001) {result[i]<-"**"} else
    if (p[i]<0.001) {result[i]<-"***"}
  }
  return(result)
}

