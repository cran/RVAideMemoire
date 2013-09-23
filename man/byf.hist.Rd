\name{byf.hist}
\alias{byf.hist}
\title{
Histogram for factor levels
}
\description{
Draw a histogram of a numeric variable per level of a factor.
}
\usage{
byf.hist(formula, data, ...)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{...}{
other arguments to pass to \code{\link{hist}}.
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