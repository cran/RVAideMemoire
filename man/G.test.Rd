\name{G.test}
\alias{G.test}
\title{
G-test
}
\description{
Perfoms a G-test on a contingency table or a vector of counts.
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

The function works as \code{\link{chisq.test}} :

- if \code{x} is a vector and theoretical proportions are not given, equality of counts is tested

- if \code{x} is a vector and theoretical proportions are given, equality of counts to theoretical counts (given by theoretical proportions) is tested

- if \code{x} is a matrix with two columns, equality of proportion of successes between populations is tested.

- if \code{x} is a matrix with more than two columns, independence of rows and columns is tested.

Since a G-test is an approximate test, an exact test is preferable when the number of individuals is small (200 is a reasonable minimum). See \code{\link[RVAideMemoire]{multinomial.test}} in that case with a vector, \code{\link{fisher.test}} with a matrix.
}
\value{
  \item{method}{name of the test.}
  \item{statistic}{test statistics.}
  \item{parameter}{test degrees of freedom.}
  \item{p.value}{p-value.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{chisq.test}}, \code{\link[RVAideMemoire]{multinomial.test}}, \code{\link{fisher.test}} \code{\link{G.multcomp}}, \code{\link{G.theo.multcomp}}, \code{\link{pairwise.G.test}}
}
\examples{
counts <- c(49,30,63,59)
G.test(counts)
}
