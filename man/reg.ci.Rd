\name{reg.ci}
\alias{reg.ci}
\title{
Confidence intervals of a simple linear regression
}
\description{
Compute and add to a graph the confidence interval of a simple regression line or of individual values.
}
\usage{
reg.ci(model, conf.level = 0.95, type = c("mean", "ind"), ...)
}
\arguments{
  \item{model}{
\code{lm} model.
}
  \item{conf.level}{
confidence level.
}
  \item{type}{
interval type : \code{"mean"} for the interval of the regression line (default), \code{"ind"} for the interval of individual values.
}
  \item{\dots}{
other agruments. See help of the \code{lines()} function.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}
}
\examples{
x<-1:50
y<-1:50+rnorm(50,0,4)
regression<-lm(y~x)
plot(x,y)
abline(regression)
reg.ci(regression,type="mean",col="red")
reg.ci(regression,type="ind",col="blue")
}