\name{reg.intcomp}
\alias{reg.intcomp}
\alias{print.reg.intcomp}
\title{
Common slope and comparison of several simple linear regression intercepts
}
\description{
Compute the common slope of parallel linear regression lines and test for equality of their intercepts. The function returns the common slope and its confidence interval, the intercept of each regression line and its confidence interval, test for equality of each intercept to a given value and performs pairwise comparisons between intercepts.
}
\usage{
reg.intcomp(formula, data, conf.level = 0.95, theo = 0, p.method = "fdr")
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
  \item{theo}{
numeric vector: theoretical value of each intercept.
}
  \item{p.method}{
method for p-values correction. See healp of the \code{p.adjust()} function.
}
}
\value{
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{conf.level}{confidence level.}
  \item{slope.comm}{common slope and confidence interval.}
  \item{intercepts}{intercept of each regression line.}
  \item{intercepts.ci}{confidence interval of intercepts.}
  \item{intercepts.theo}{theoretical intercepts.}
  \item{intercepts.t}{test statistics for equality to theoretical values.}
  \item{intercepts.p}{corrected p-values for equality to theoretical values.}
  \item{intercepts.tab}{data frame of results of tests for equality to theoretical value.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{multcomp}{table of results of pairwise comparisons.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}
}
\examples{
covariable <- rep(1:30,3)
variable <- c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
fact <- gl(3,30,labels=LETTERS[1:3])
reg.intcomp(variable~covariable|fact)
}