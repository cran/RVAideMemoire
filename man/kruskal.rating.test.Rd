\name{kruskal.rating.test}
\alias{kruskal.rating.test}
\alias{kruskal.rating.test.default}
\alias{kruskal.rating.test.formula}
\title{
Kruskal-Wallis rank sum test for ratings
}
\description{
Wrapper for \code{\link{kruskal.test}} with ratings (ordinal response variables).
}
\usage{

kruskal.rating.test(x, ...)

\method{kruskal.rating.test}{default}(x, g, ...)

\method{kruskal.rating.test}{formula}(formula, data, subset, na.action, ...)
}
\arguments{
  \item{x}{
response variable (preferably an ordered factor).
}
  \item{g}{
a factor giving the group for the corresponding element of \code{x}.
}
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} and \code{b} give the data values and corresponding groups. It is preferable that \code{a} is an ordered factor.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{subset}{
an optional vector specifying a subset of observations to be used.
}
  \item{na.action}{
a function which indicates what should happen when the data contain \code{NA}s. Defaults to \code{getOption("na.action")}.
}
  \item{...}{
further arguments to be passed to or from other methods.
}
}
\value{
See \code{\link{kruskal.test}}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
set.seed(1609)
response <- factor(sample(1:6,30,replace=TRUE),levels=c("1","2","3","4","5","6"),ordered=TRUE)
fact <- gl(3,1,30,labels=LETTERS[1:3])
kruskal.rating.test(response~fact)
}