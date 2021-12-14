\name{quasibinomial.QAIC}
\alias{quasibinomial.QAIC}
\alias{quasipoisson.QAIC}
\title{
QAIC(c) calculation with GLMs of the quasi- family
}
\description{
Allows for the calculation of QAIC(c) with \code{glm} models built with a \code{quasibinomial} or \code{quasipoisson} distribution. The function is directly coming from \link[MuMIn]{QAIC}.
}
\usage{
quasibinomial.QAIC(link = "logit")
quasipoisson.QAIC(link = "log")
}
\arguments{
  \item{link}{see \link{family} and \link[MuMIn]{QAIC}.}
}
\author{
This function is only intended to make the suggestion of Kamil Barton in \link[MuMIn]{QAIC} available.
}
\examples{
# See ?QAIC from the MuMIn package
}