\name{byf.qqnorm}
\alias{byf.qqnorm}
\title{
QQ-plot for factor levels
}
\description{
Draws a QQ-plot of a numeric variable per level of a factor.
}
\usage{
byf.qqnorm(formula, data, ...)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{...}{
other arguments to pass to \code{\link[car]{qqPlot}}.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{link[RVAideMemoire]{byf.shapiro}}, \code{\link[car]{qqPlot}}
}
\examples{
data(iris)
byf.qqnorm(Sepal.Length~Species,data=iris)
}