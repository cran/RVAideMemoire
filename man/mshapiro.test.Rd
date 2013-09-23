\name{mshapiro.test}
\alias{mshapiro.test}
\title{
Shapiro-Wilk multivariate normality test
}
\description{
Performs a Shapiro-Wilk test to asses multivariate normality. This is a slightly modified copy of the \code{\link[mvnormtest]{mshapiro.test}} function of the package mvnormtest, for internal convenience.
}
\usage{
mshapiro.test(x)
}
\arguments{
  \item{x}{
a data frame or a matrix of numeric variables (each column giving a variable).
}
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the names of the data.}
  \item{statistic}{test statistics of the test.}
  \item{p.value}{p-value of the test.}
}
\author{
Maxime Hervé <mx.herve@gmail.com> from the work of Slawomir Jarek
}
\seealso{
\code{\link{shapiro.test}}, \code{\link[mvnormtest]{mshapiro.test}}
}
\examples{
x <- 1:30+rnorm(30)
y <- 1:30+rnorm(30,1,3)
mshapiro.test(cbind(x,y))
}
