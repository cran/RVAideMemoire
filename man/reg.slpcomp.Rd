\name{reg.slpcomp}
\alias{reg.slpcomp}
\alias{print.reg.slpcomp}
\title{
Comparison of several simple linear regression slopes
}
\description{
Test for equality of slopes of several simple regression lines. The function returns the confidence interval of each slope and perfoms pairwise comparisons.
}
\usage{
reg.slpcomp(formula, data, conf.level = 0.95, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{y ~ x | f}, where \code{y} and \code{x} give the dependent and independent variable, respectively, and \code{f} is a factor giving levels to be compared.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{conf.level}{
confidence level.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\value{
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{conf.level}{confidence level.}
  \item{coeffs}{slope of each regression line.}
  \item{coeffs.tab}{slope and confidence interval of each regression line.}
  \item{p.adjust.method}{method for p-value correction.}
  \item{multcomp}{table of results of pairwise comparisons.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}
}
\examples{
x <- rep(1:30,3)
y <- c(seq(1,10,9/29)+rnorm(30,0,3),seq(1,30,1)+rnorm(30,0,3),
  seq(-1,-80,-79/29)+rnorm(30,0,3))
fact <- gl(3,30,labels=LETTERS[1:3])
reg.slpcomp(y~x|fact)
}