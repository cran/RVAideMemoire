\name{cdf.discrete}
\alias{cdf.discrete}
\title{
Cumulative Distribution Function of a known discrete distribution
}
\description{
Returns an object similar to what is produced by \code{\link{ecdf}}, but based on a known discrete distribution.
}
\usage{
cdf.discrete(x, dist = c("binom", "geom", "hyper", "nbinom", "pois"), ...)
}
\arguments{
  \item{x}{
numeric vector of the observations.
}
  \item{dist}{
character string naming a discrete distribution (\code{"binom"} by default).
}
  \item{\dots}{
parameters of the distribution specified by \code{dist}.
}
}
\details{
The function is intended to be used in goodness-of-fits tests for discrete distributions, such as proposed in the \code{dgof} package.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
if(require(dgof)) {
  set.seed(1124)
  resp <- rpois(20,2)
  cvm.test(resp,cdf.discrete(resp,"pois",2))
}
}