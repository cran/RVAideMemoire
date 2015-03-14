\name{CvM.test}
\alias{CvM.test}
\title{
Two-sample Cramer-von Mises test
}
\description{
Wrapper for \code{\link[cramer]{cramer.test}}, for more convenient result printing.
}
\usage{
CvM.test(x, y, ...)
}
\arguments{
  \item{x}{
a numeric vector of data values.
}
  \item{y}{
a numeric vector of data values.
}
  \item{\dots}{
additional arguments to \code{\link[cramer]{cramer.test}}. See help of this function.
}
}
\details{
See help of the \code{\link[cramer]{cramer.test}} function for more explanations.
}
\value{
  \item{statistic}{test statistics.}
  \item{p.value}{p-value of the test.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the names of the data.}
}
\author{
Maxime Hervé <mx.herve@gmail.com> based on the function of Carsten Franz
}
\examples{
set.seed(1109)
x <- rpois(30,2)
y <- rpois(30,3)
CvM.test(x,y)
}