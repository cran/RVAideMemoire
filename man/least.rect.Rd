\name{least.rect}
\alias{least.rect}
\title{
Least rectangles linear regression
}
\description{
Fits a least rectangle linear regression, possibly for each level of a factor.
}
\usage{
least.rect(formula, data, conf.level = 0.95, theo = 1, adj = TRUE)
}
\arguments{
  \item{formula}{
a formula of the form \code{y ~ x}, where \code{y} and \code{x} give the y and x variable, respectively. The formula can also be \code{y ~ x | f} to fit a (separate) regression for each level of the factor \code{f}.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{conf.level}{
confidence level.
}
  \item{theo}{
theoretical value of the slope. If several regression are fitted, the same value is used for all comparisons of slope vs. theoretical value.
}
  \item{adj}{
logical indicating if, in case of several regressions fitted, confidence intervals and p-values should be Bonferroni-corrected for multiple testing.
}
}
\value{
  \item{coefficients}{regression parameters.}
  \item{residuals}{residuals.}
  \item{fitted.values}{fitted values.}
  \item{call}{the matched call.}
  \item{model}{the model frame used.}
  \item{conf.level}{confidence level.}
  \item{conf.int}{confidence interval of regression parameters.}
  \item{theo}{theoretical value of the slope.}
  \item{comp}{data frame of results for equality of the slope(s) to the theoretical value.}
  \item{corr}{data frame of results for significativity of the correlation coefficient(s).}
  \item{multiple}{logical, \code{TRUE} if several regressions are fitted.}
  \item{adj}{logical, \code{TRUE} if confidence intervals and p-values are corrected for multiple testing (only if several regressions are fitted).}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
x <- 1:30+rnorm(30,0,3)
y <- 1:30+rnorm(30,0,3)
regression1 <- least.rect(y~x)
summary(regression1)

x2 <- c(1:30,1:30)
y2 <- c(1:30+rnorm(30,0,3),seq(10,22,12/29)+rnorm(30,0,3))
fact <- gl(2,30,labels=LETTERS[1:2])
regression2 <- least.rect(y2~x2|fact)
summary(regression2)
}