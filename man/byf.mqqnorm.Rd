\name{byf.mqqnorm}
\alias{byf.mqqnorm}
\title{
QQ-plot for factor levels
}
\description{
Draws a multivariate QQ-plot of numeric variables per level of a factor.
}
\usage{
byf.mqqnorm(formula, data)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} is a matrix giving the dependent variables (each column giving a variable) and \code{b} a factor giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{mqqnorm}}, \code{\link[RVAideMemoire]{byf.mshapiro}}, \code{\link[car]{qqPlot}}
}
\examples{
data(iris)
byf.mqqnorm(as.matrix(iris[,1:4])~Species,data=iris)
}