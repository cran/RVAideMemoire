\name{multinomial.test}
\alias{multinomial.test}
\title{
Exact multinomial test
}
\description{
Perfoms an exact multinomial test on a vector of counts.
}
\usage{
multinomial.test(x, p = rep(1/length(x), length(x)))
}
\arguments{
  \item{x}{
numeric vector (counts). Can also be a factor; in that case \code{table(x)} is used as counts.
}
  \item{p}{
theoretical proportions (optional).
}
}
\details{
The function works as \code{\link{chisq.test}} or \code{\link[RVAideMemoire]{G.test}} :

- if theoretical proportions are not given, equality of counts is tested

- if theoretical proportions are given, equality of counts to theoretical counts (given by theoretical proportions) is tested.

Since chi-squared and G tests are approximate tests, exact tests are preferable when the number of individuals is small (200 is a reasonable minimum).

Be aware that the calculation time increases with the number of individuals (i.e. the sum of \code{x}) and the number of groups (i.e. the length of \code{x}).

An exact multinomial test with two groups is strictly the same as an exact binomial test.
}
\value{
  \item{method}{name of the test.}
  \item{p.value}{p-value.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{the observed counts.}
  \item{expected}{the expected counts under the null hypothesis.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr> based on \code{\link[EMT]{multinomial.test}}
}
\seealso{
\code{\link{chisq.test}}, \code{\link[RVAideMemoire]{G.test}}, \code{\link{binom.test}}, \code{\link[RVAideMemoire]{multinomial.multcomp}}, \code{\link[RVAideMemoire]{multinomial.theo.multcomp}}
}
\examples{
counts <- c(5,15,23)
multinomial.test(counts)
}
