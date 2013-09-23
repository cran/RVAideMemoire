\name{byf.normhist}
\alias{byf.normhist}
\title{
Histogram and normal distribution for factor levels
}
\description{
Draw on the same histogram of a numeric variable a histogram per level of a factor, and add the normal distribution line of each level.
}
\usage{
byf.normhist(formula, data)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
data(iris)
byf.normhist(Sepal.Length~Species,data=iris)
}