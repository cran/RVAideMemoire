\name{perm.var.test}
\alias{perm.var.test}
\alias{perm.var.test.default}
\alias{perm.var.test.formula}
\title{
permutation F test to compare two variances
}
\description{
Performs a permutation F test to compare two variances.
}
\usage{

perm.var.test(x, ...)

\method{perm.var.test}{default}(x, y, ...)

\method{perm.var.test}{formula}(formula, data, alternative = c("two.sided", "less",
  "greater"), nperm = 999, progress = TRUE, ...)
}
\arguments{
  \item{x}{
a numeric vector of data values.
}
  \item{y}{
a numeric vector of data values.
}
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor with 2 levels giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
}
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
  \item{\dots}{
further arguments to be passed to or from other methods.
}
}
\details{
The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{method}{name of the test.}
  \item{statistic}{test statistics of the parametric test.}
  \item{permutations}{number of permutations.}
  \item{p.value}{p-value of the permutation test.}
  \item{estimate}{the ratio of the two variances.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{null.value}{the ratio of population variances under the null hypothesis, always 1.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{var.test}}
}
\examples{
response <- c(rpois(8,1),rpois(8,3))
fact <- gl(2,8,labels=LETTERS[1:2])
perm.var.test(response~fact)
}