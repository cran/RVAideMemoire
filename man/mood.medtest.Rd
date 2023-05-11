\name{mood.medtest}
\alias{mood.medtest}
\alias{mood.medtest.default}
\alias{mood.medtest.formula}
\title{
Mood's median test
}
\description{
Performs a Mood's median test to compare medians of independent samples.
}
\usage{

mood.medtest(x, ...)

\method{mood.medtest}{default}(x, g, exact = NULL, ...)

\method{mood.medtest}{formula}(formula, data, subset, ...)
}
\arguments{
  \item{x}{
a numeric vector of data values.
}
  \item{g}{
a vector or factor object giving the group for the corresponding elements of \code{x}.
}
  \item{exact}{
a logical indicating whether an exact p-value should be computed.
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
If \code{exact=NULL}, a Fisher's exact test is used if the number of data values is < 200; otherwise a chi-square test is used, with Yates continuity correction if necessary.
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{statistic}{the value the chi-squared test statistic (in case of a chis-square test).}
  \item{parameter}{the degrees of freedom of the approximate chi-squared distribution of the test statistic (in case of a chis-square test).}
  \item{p.value}{the p-value of the test.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
set.seed(1716)
response <- c(rnorm(10,3,1.5),rnorm(10,5.5,2))
fact <- gl(2,10,labels=LETTERS[1:2])
mood.medtest(response~fact)
}