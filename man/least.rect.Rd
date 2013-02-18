\name{least.rect}
\alias{least.rect}
\alias{print.least.rect}
\title{
Least rectangles linear regression
}
\description{
Return information about a least rectangle linear regression: equation, confidence interval of parameters, equality of the slope to a given value and Pearson's linear correlation coefficient.
}
\usage{
least.rect(formula, data, conf.level = 0.95, theo = 1)
}
\arguments{
  \item{formula}{
a formula of the form \code{y ~ x}, where \code{y} and \code{x} give the y and x variable, respectively.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{conf.level}{
confidence level.
}
  \item{theo}{
theoretical value of the slope.
}
}
\value{
  \item{coefficients}{regression parameters.}
  \item{x}{name of the x variable.}
  \item{y}{name of the y variable.}
  \item{residuals}{residuals.}
  \item{fitted.values}{fitted values.}
  \item{conf.level}{confidence level.}
  \item{conf.int}{confidence interval of regression parameters.}
  \item{slope.theo}{theoretical value of the slope.}
  \item{df.comp}{degrees of freedom for equality of the slope to the theoretical value.}
  \item{statistic.comp}{test statitics for equality of the slope to the theoretical value.}
  \item{p.value.comp}{p-value for equality of the slope to the theoretical value.}
  \item{model}{values of x and y variables.}
  \item{comp}{data frame of results for equality of the slope to the theoretical value.}
  \item{r}{Pearson's linear correlation coefficient and confidence interval.}
  \item{r.df}{degrees of freedom for significativity of the correlation coefficient.}
  \item{r.statistic}{test statistics for significativity of the correlation coefficient.}
  \item{r.p.value}{p-value for significativity of the correlation coefficient.}
  \item{corr}{data frame of results for significativity of the correlation coefficient.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{cor.test}}
}
\examples{
x <- 1:30+rnorm(30,0,3)
y <- 1:30+rnorm(30,0,3)
least.rect(y~x)
}