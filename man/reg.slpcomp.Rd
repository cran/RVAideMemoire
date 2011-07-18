\name{reg.slpcomp}
\alias{reg.slpcomp}
\title{
Comparison of simple linear regression slopes
}
\description{
Test for equality of slopes of several simple regression lines. The function returns the confidence interval of each slope and perfoms pairwise comparisons.
}
\usage{
reg.slpcomp(var, covar, fact, conf.level = 0.95, p.method = "fdr")
}
\arguments{
  \item{var}{
numeric vector (response variable).
}
  \item{covar}{
numeric vector (covariable).
}
  \item{fact}{
factor (groups).
}
  \item{conf.level}{
confidence level.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{conf.level}{confidence level.}
  \item{coeffs}{slope of each regression line.}
  \item{coeffs.tab}{slope and confidence interval of each regression line.}
  \item{p.method}{method for p-value correction.}
  \item{df}{degrees of freedom of each comparison.}
  \item{t}{test statistics.}
  \item{p.value}{corrected p-values.}
  \item{multcomp}{data.frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}
}
\examples{
covariable<-rep(1:30,3)
variable<-c(seq(1,10,9/29)+rnorm(30,0,3),seq(1,30,1)+rnorm(30,0,3),seq(-1,-80,-79/29)+rnorm(30,0,3))
factor<-factor(rep(LETTERS[1:3],each=30))
reg.slpcomp(variable,covariable,factor)
}