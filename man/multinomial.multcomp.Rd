\name{multinomial.multcomp}
\alias{multinomial.multcomp}
\title{
Pairwise comparisons after an exact multinomial test
}
\description{
Performs pairwise comparisons after a global exact multinomial test. These comparisons are performed using exact binomial tests.
}
\usage{
multinomial.multcomp(x, p.method = "fdr")
}
\arguments{
  \item{x}{
numeric vector (counts). Can also be a factor; in that case \code{table(x)} is used as counts.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
Since chi-squared and G tests are approximate tests, exact tests are preferable when the number of individuals is small (200 is a reasonable minimum).

An exact multinomial test with two groups is strictly the same than an exact binomial test.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{multinomial.test}}, \code{\link{binom.test}}
}
\examples{
counts <- c(5,15,23)
multinomial.test(counts)
multinomial.multcomp(counts)
}