\name{byf.mshapiro}
\alias{byf.mshapiro}
\title{
Shapiro-Wilk test for factor levels
}
\description{
Performs a multivariate Shapiro-Wilk test on numeric variables per level of a factor.
}
\usage{
byf.mshapiro(formula, data)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} is a matrix giving the dependent variables (each column giving a variable) and \code{b} a factor giving the corresponding groups.
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
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{byf.mqqnorm}}, \code{\link[RVAideMemoire]{mshapiro.test}}, \code{\link[car]{qqPlot}}
}
\examples{
data(iris)
byf.mshapiro(as.matrix(iris[,1:4])~Species,data=iris)
}