mqqnorm <- function(x,main="Multi-normal Q-Q Plot") {
  x <- t(x)
  distances <- mahalanobis(x,colMeans(x),cov(x))
  qqPlot(distances,distribution="chisq",df=mean(distances),lwd=1,grid=FALSE,
    main=main,xlab=expression(chi^2 * " quantiles"),
    ylab=expression("Mahalanobis distances "^2))
}
