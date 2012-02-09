\name{perm.cor.test}
\alias{perm.cor.test}
\title{
Permutational Pearson's correlation test
}
\description{
Perform a permutational Pearson's product-moment correlation test.
}
\usage{
perm.cor.test(x, y, alternative = c("two.sided", "less", "greater"), nperm = 999)
}
\arguments{
  \item{x, y}{
numeric vectors of data values. \code{x} and \code{y} must have the same length.
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
}
  \item{nperm}{
number of permutations.
}
}
\value{
  \item{statistic}{test statistics of the parametric test.}
  \item{permutations}{number of permutations.}
  \item{p.value}{p-value of the permutational test.}
  \item{estimate}{the estimated correlation coefficient.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{H1}{a character string giving the explicit alternative hypothesis.}
  \item{data.name}{a character string giving the names of the data.}
  \item{conf.int}{the 95\% confidence interval for the correlation coefficient.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{cor.test}}
}
\examples{
x <- rnorm(50)
y <- runif(50)
perm.cor.test(x,y)
}