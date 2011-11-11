\name{lr.multcomp}
\alias{lr.multcomp}
\title{
Comparison of simple linear least rectangles regression lines
}
\description{
Compare confidence intervals of parameters of several simple linear least rectangles regression lines. No test is performed, the function just compares confidence intervals and return 'equal' or 'unequal'.
}
\usage{
lr.multcomp(formula, data=NULL, conf.level = 0.95)
}
\arguments{
  \item{formula}{
a formula of the form \code{y ~ x | f}, where \code{y} and \code{x} give the y and x variable, respectively, and \code{f} is a factor giving levels to be compared.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{conf.level}{
confidence level.
}
}
\value{
  \item{data}{variables name.}
  \item{conf.level}{confidence level.}
  \item{n.reg}{number of regression lines.}
  \item{intercepts}{intercept and confidence interval of each regression line.}
  \item{slopes}{slope and confidence interval of each regression lines.}
  \item{comp}{data frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{least.rect}}
}
\examples{
x<-rep(1:30,3)
y<-c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
factor<-as.factor(rep(LETTERS[1:3],each=30))
lr.multcomp(y~x|factor)
}
