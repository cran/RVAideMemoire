\name{se}
\alias{se}
\title{
Standard error
}
\description{
Computes the standard error of a mean or of a proportion.
}
\usage{
se(x, y = NULL)
}
\arguments{
  \item{x}{
numeric vector or number of successes.
}
  \item{y}{
number of trials. If \code{NULL}, the standard error of the mean of \code{x} is computed. If not, the standard error of the proportion \code{x/y} is computed.
}
}
\details{
The function deals with missing values.

For the standard error of a proportion, \code{y - 1} is used as denominator.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
# Standard error of a mean
se(rnorm(30))

# Standard error of a proportion
se(9,25)
}