\name{byf.shapiro}
\alias{byf.shapiro}
\title{
Shapiro-Wilk test for factor levels
}
\description{
Perform a Shapiro-Wilk test on a numeric variable per level of a factor.
}
\usage{
byf.shapiro(formula, data)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the names of the data.}
  \item{tab}{table of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{byf.qqnorm}}, \code{\link{shapiro.test}}
}
\examples{
data(iris)
byf.shapiro(Sepal.Length~Species,data=iris)
}