\name{summary.least.rect}
\alias{summary.least.rect}
\title{
Summary method for least rectangles linear regression
}
\description{
Summary method for least rectangles linear regression. Returns the regression coefficients, the test(s) for equality of the slope(s) to a given value and information about the correlation between the two variables (value, confidence interval and significativiy).
}
\usage{
\method{summary}{least.rect}(object, ...)
}
\arguments{
  \item{object}{
regression fitted by \code{\link[RVAideMemoire]{least.rect}}.
}
  \item{...}{
further arguments.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{least.rect}}
}
\examples{
x <- 1:30+rnorm(30,0,3)
y <- 1:30+rnorm(30,0,3)
regression1 <- least.rect(y~x)
summary(regression1)

x2 <- c(1:30,1:30)
y2 <- c(1:30+rnorm(30,0,3),seq(10,22,12/29)+rnorm(30,0,3))
fact <- gl(2,30,labels=LETTERS[1:2])
regression2 <- least.rect(y2~x2|fact)
summary(regression2)
}
