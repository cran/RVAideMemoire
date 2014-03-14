\name{friedman.rating.test}
\alias{friedman.rating.test}
\alias{friedman.rating.test.default}
\alias{friedman.rating.test.formula}
\title{
Friedman rank sum test for ratings
}
\description{
Wrapper for \code{\link{friedman.test}} with ratings (ordinal response variables).
}
\usage{

friedman.rating.test(y, ...)

\method{friedman.rating.test}{default}(y, groups, blocks, ...)

\method{friedman.rating.test}{formula}(formula, data, subset, na.action, ...)
}
\arguments{
  \item{y}{
response variable (preferably an ordered factor).
}
  \item{groups}{
a vector giving the group for the corresponding element of \code{y}.
}
  \item{blocks}{
a vector giving the block for the corresponding element of \code{y}.
}
  \item{formula}{
a formula of the form \code{a ~ b | c}, where \code{a}, \code{b} and \code{c} give the data values and corresponding groups and blocks, respectively. It is preferable that \code{a} is an ordered factor.
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
See \code{\link{friedman.test}}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
set.seed(1608)
response <- factor(sample(1:6,30,replace=TRUE),levels=c("1","2","3","4","5","6"),ordered=TRUE)
fact <- gl(3,1,30,labels=LETTERS[1:3])
block <- gl(10,3,labels=letters[1:10])
friedman.rating.test(response~fact|block)
}