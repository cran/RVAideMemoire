\name{wilcox.rating.test}
\alias{wilcox.rating.test}
\alias{wilcox.rating.test.default}
\alias{wilcox.rating.test.formula}
\title{
Wilcoxon rank sum and signed rank tests for ratings
}
\description{
Wrapper for \code{\link{wilcox.test}} with ratings (ordinal response variables).
}
\usage{

wilcox.rating.test(x, ...)

\method{wilcox.rating.test}{default}(x, y = NULL, mu = NULL, ...)

\method{wilcox.rating.test}{formula}(formula, data, subset, na.action, ...)
}
\arguments{
  \item{x}{
response variable (preferably an ordered factor).
}
  \item{y}{
an optional second response variable (preferably an ordered factor) to be compared with \code{x}.
}
  \item{mu}{
a character string giving a theoretical rating to be compared with \code{x} in one-sample tests.
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
See \code{\link{wilcox.test}}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
set.seed(1719)
response <- factor(sample(1:6,30,replace=TRUE),levels=c("1","2","3","4","5","6"),ordered=TRUE)
fact <- gl(2,15,labels=LETTERS[1:2])
wilcox.rating.test(response~fact)
}