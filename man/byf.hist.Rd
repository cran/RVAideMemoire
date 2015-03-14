\name{byf.hist}
\alias{byf.hist}
\title{
Histogram for factor levels
}
\description{
Draws a histogram of a numeric variable per level of a factor.
}
\usage{
byf.hist(formula, data, sep = FALSE, density = TRUE)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{sep}{
logical. If \code{TRUE} a histogram is displayed per level of the factor. If \code{FALSE} all levels are displayed on the same histogram.
}
  \item{density}{
logical. If \code{TRUE} density polygons are displayed, if \code{FALSE} classical counts are displayed.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{hist}}
}
\examples{
data(iris)
byf.hist(Sepal.Length~Species,data=iris)
}