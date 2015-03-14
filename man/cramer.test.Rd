\name{cramer.test}
\alias{cramer.test}
\title{
Cramer's association coefficient
}
\description{
Computes the Cramér's association coefficient between 2 nominal variables, its confidence interval (by bootstraping) and tests for its significance.
}
\usage{
cramer.test(x, y, nrep = 1000, conf.level = 0.95)
}
\arguments{
  \item{x}{
a contingency table ('matrix' or 'table' object). \code{x} and \code{y} can also both be factors.
}
  \item{y}{
ignored if \code{x} is a contingency table. If not, \code{y} should be a vector of the same length.
}
  \item{nrep}{
number of replicates for bootstraping.
}
  \item{conf.level}{
confidence level.
}
}
\value{
  \item{method}{name of the test.}
  \item{statistic}{test statistics.}
  \item{parameter}{test degrees of freedom.}
  \item{p.value}{test p-value.}
  \item{data.name}{a character string giving the names of the data.}
  \item{estimate}{Cramér's coefficient.}
  \item{conf.level}{confidence level.}
  \item{rep}{number of replicates.}
  \item{conf.int}{confidence interval.}
  \item{alternative}{a character string giving the alternative hypothesis, always \code{"two.sided"}}
  \item{null.value}{the value of the association measure under the null hypothesis, always 0.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[boot]{boot}}
}
\examples{
var1 <- sample(LETTERS[1:3],30,replace=TRUE)
var2 <- sample(letters[1:3],30,replace=TRUE)
cramer.test(var1,var2)
# or cramer.test(table(var1,var2))
}