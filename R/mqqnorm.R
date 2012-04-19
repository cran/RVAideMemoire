mqqnorm <-
function(x) {
  x <- t(x)
  distances <- mahalanobis(x,colMeans(x),cov(x))
  qqplot(qchisq(ppoints(nrow(x)),df=ncol(x)),distances,main="Multi-normal Q-Q Plot",
    xlab=expression(chi^2 * " quantiles"),ylab=expression("Mahalanobis distances "^2))
}
