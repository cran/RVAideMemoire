\name{wilcox.signtest}
\alias{wilcox.signtest}
\alias{wilcox.signtest.default}
\alias{wilcox.signtest.formula}
\title{
Wilcoxon sign test
}
\description{
Performs a Wilcoxon sign test to compare medians of two paired samples or one median to a given value.
}
\usage{

wilcox.signtest(x, ...)

\method{wilcox.signtest}{default}(x, y = NULL, mu = 0, conf.level = 0.95, ...)

\method{wilcox.signtest}{formula}(formula, data, subset, ...)
}
\arguments{
  \item{x}{
a numeric vector of data values.
}
  \item{y}{
an optional numeric vector of data values (for paired two-sample test).
}
  \item{mu}{
theoretical median (one-sample test) or theoretical median of \code{x-y} differences.
}
  \item{conf.level}{
confidence level of the interval.
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
If zeroes (i.e. null differences with \code{mu}) are present, the median of the data different from \code{mu} is tested in the one-sample situation; the median of the \code{x-y} differences different from \code{mu} in the two-sample situation.
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{null.value}{the specified hypothesized value of the median or median difference depending on the test performed.}
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string giving the alternative hypothesis, always \code{"two.sided"}}
  \item{estimate}{the estimated median or median of \code{x-y} differences, depending on the test performed.}
  \item{conf.int}{a confidence interval for the median tested.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{wilcox.test}}
}
\examples{
set.seed(1706)
response <- c(rnorm(7,3,1.5),rnorm(7,5.5,2))

# Comparison of 2 samples
fact <- gl(2,7,labels=LETTERS[1:2])
wilcox.signtest(response~fact)

# Comparison to a given value
theo <- 4
wilcox.signtest(response,mu=theo)
}