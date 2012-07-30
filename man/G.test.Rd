\name{G.test}
\alias{G.test}
\title{
G-test
}
\description{
Perfom a G-test on a contingency table or a vector of counts.
}
\usage{
G.test(x, p = rep(1/length(x), length(x)))
}
\arguments{
  \item{x}{
a numeric vector or matrix (see Details).
}
  \item{p}{
theoretical proportions (optional).
}
}
\details{
If \code{x} is matrix, it must be constructed like this:

- 2 columns giving number of successes (left) and fails (right)

- 1 row per population.

The function works as \code{chisq.test()} :

- if \code{x} is a vector and theoretical proportions are not given, equality of counts is tested

- if \code{x} is a vector and theoretical proportions are given, equality of counts to theoretical counts (given by theoretical proportions) is tested

- if \code{x} is a matrix, equality of proportion of successes between populations is tested.
}
\value{
  \item{statistic}{test statistics.}
  \item{parameter}{test degrees of freedom}
  \item{p.value}{p-value.}
  \item{method}{a character string indicating the type of test performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{chisq.test}}, \code{\link{G.multcomp}}, \code{\link{G.theo.multcomp}}, \code{\link{pairwise.G.test}}
}
