\name{cor.conf}
\alias{cor.conf}
\title{
Equality of a Pearson's linear correlation coefficient to a given value
}
\description{
Performs a test for equality of a Pearson's linear correlation coefficient to a given value.
}
\usage{
cor.conf(var1, var2, theo)
}
\arguments{
  \item{var1}{
numeric vector (first variable).
}
  \item{var2}{
numeric vector (second variable).
}
  \item{theo}{
theoretical value.
}
}
\value{
  \item{method}{a character string giving the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{statistic}{test statistics.}
  \item{p.value}{p-value of the test.}
  \item{null.value}{the value of the theoretical coefficient.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{estimate}{the estimated correlation coefficient.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{cor.test}}
}
\examples{
var1 <- 1:30+rnorm(30,0,4)
var2 <- 1:30+rnorm(30,0,4)
cor.conf(var1,var2,theo=0.5)
}