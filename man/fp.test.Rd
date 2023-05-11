\name{fp.test}
\alias{fp.test}
\alias{fp.test.default}
\alias{fp.test.formula}
\title{
Fligner-Policello test
}
\description{
Performs a Fligner-Policello test of the null that the medians in the two groups (samples) are the same.
}
\usage{

fp.test(x, ...)

\method{fp.test}{default}(x, y, delta = 0, alternative = "two.sided", ...)

\method{fp.test}{formula}(formula, data, subset, ...)
}
\arguments{
  \item{x}{
a numeric vector of data values.
}
  \item{y}{
a numeric vector of data values.
}
  \item{delta}{
null difference in medians tested.
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
}
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} and \code{b} give the data values and corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{subset}{
an optional vector specifying a subset of observations to be used.
}
  \item{...}{
further arguments to be passed to or from other methods.
}
}
\details{
The Fligner-Policello test does not assume that the shape of the distribution is similar in two groups, contrary to the Mann-Whitney-Wilcoxon test. However, it assumes that the the distributions are symmetric.
}
\value{
  \item{statistic}{test statistics.}
  \item{p.value}{p-value of the test.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the names of the data.}
  \item{null.value}{the specified hypothesized value of the median difference.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{wilcox.test}}
}
\examples{
x <- rpois(20,3)
y <- rpois(20,5)
fp.test(x,y)
}