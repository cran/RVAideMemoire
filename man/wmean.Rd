\name{wmean}
\alias{wmean}
\title{
Weighted arithmetic mean
}
\description{
Computes the weighted arithmetic mean of a vector.
}
\usage{
wmean(x, w = rep(1, length(x)), na.rm = TRUE)
}
\arguments{
  \item{x}{
numeric vector.
}
  \item{w}{
numeric vector of weights.
}
  \item{na.rm}{
a logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
mean(1:10)
wmean(1:10,w=10:1)
}