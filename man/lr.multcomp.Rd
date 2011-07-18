\name{lr.multcomp}
\alias{lr.multcomp}
\title{
Comparison of simple linear least rectangles regression lines
}
\description{
Compare confidence intervals of parameters of several simple linear least rectangles regression lines. No test is performed, the function just compares confidence intervals and return 'equal' or 'unequal'.
}
\usage{
lr.multcomp(x, y, fact, conf.level = 0.95)
}
\arguments{
  \item{x}{
numeric vector (variable drawn in abscissa).
}
  \item{y}{
numeric vector (variable drawn in ordinate).
}
  \item{fact}{
factor (groups).
}
  \item{conf.level}{
confidence level.
}
}
\value{
  \item{conf.level}{confidence level.}
  \item{n.reg}{number of regression lines.}
  \item{intercepts}{intercept and confidence interval of each regression line.}
  \item{slopes}{slope and confidence interval of each regression lines.}
  \item{comp}{data.frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{least.rect}}
}
\examples{
covariable<-rep(1:30,3)
variable<-c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
factor<-as.factor(rep(LETTERS[1:3],each=30))
lr.multcomp(variable,covariable,factor)
}
