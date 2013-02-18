\name{byf.mqqnorm}
\alias{byf.mqqnorm}
\title{
QQ-plot for factor levels
}
\description{
Draw a multivariate QQ-plot of numeric variables per level of a factor.
}
\usage{
byf.mqqnorm(resp, fact)
}
\arguments{
  \item{resp}{
a data frame or a matrix of numeric variables (each column giving a variable).
}
  \item{fact}{
a factor giving the groups to be plotted.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{mqqnorm}}, \code{\link[car]{qqPlot}}
}
\examples{
data(iris)
byf.mqqnorm(iris[,1:4],iris$Species)
}