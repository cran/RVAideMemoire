# FactoMineR: GPA

GPA.test <- function(df,group,tolerance=10^-10,nbiteration=200,scale=TRUE,nperm=999,progress=TRUE) {
  dname <- paste0(deparse(substitute(df)),"\n",nperm," permutations")
  GPA.ref <- FactoMineR::GPA(df,group,tolerance=tolerance,nbiteration=nbiteration,scale=scale,graph=FALSE)
  ref <- GPA.ref$PANOVA$dimension["Total","Consensus"]
  names(ref) <- "Rc"
  stat.perm <- numeric(nperm+1)
  stat.perm[1] <- ref
  if (progress) {pb <- txtProgressBar(min=0,max=100,initial=0,style=3)}
  n <- nrow(df)
  rownames(df) <- 1:n
  tab.list <- list()
  tab.list[[1]] <- df[,1:group[1]]
  df <- df[,-c(1:group[1])]
  for (i in 2:length(group)) {
    tab.list[[i]] <- df[,1:group[i]]
    df <- df[,-c(1:group[i])]
  }
  for(i in 1:nperm) {
    if (progress) {setTxtProgressBar(pb,round(i*100/nperm,0))}
    df.temp <- do.call("cbind",lapply(tab.list,function(x) {x[sample(1:n),]}))
    GPA.perm <- FactoMineR::GPA(df.temp,group,tolerance=tolerance,nbiteration=nbiteration,scale=scale,graph=FALSE)
    stat.perm[i] <- GPA.perm$PANOVA$dimension["Total","Consensus"]
   }
  if (progress) {cat("\n")}
  pvalue <- length(which((stat.perm+.Machine$double.eps/2) >= ref))/(nperm+1)
  result <- list(method="Permutation test based on consensus variance",data.name=dname,statistic=ref,permutations=nperm,
    p.value=pvalue)
  class(result) <- "htest"
  return(result)
}
