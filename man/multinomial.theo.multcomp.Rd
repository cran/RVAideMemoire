\name{multinomial.theo.multcomp}
\alias{multinomial.theo.multcomp}
\title{
Pairwise comparisons after an exact multinomial test for given probabilities
}
\description{
Performs pairwise comparisons after a global exact multinomial test for given probabilities. These comparisons are performed using exact binomial tests.
}
\usage{
multinomial.theo.multcomp(x, p = rep(1/length(x), length(x)), prop = FALSE,
  p.method = "fdr")
}
\arguments{
  \item{x}{
numeric vector (counts). Can also be a factor; in that case \code{table(x)} is used as counts.
}
  \item{p}{
theoretical proportions.
}
  \item{prop}{
logical indicating if results should be printed as counts (\code{FALSE}) or proportions (\code{TRUE}).
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
  \item{observed}{observed counts.}
  \item{expected}{expected counts.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value2}{corrected p-values.}
  \item{p.value}{data frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{multinomial.test}}, \code{\link{binom.test}}
}
\examples{
counts <- c(5,15,23)
p.theo <- c(0.2,0.5,0.3)
multinomial.test(counts,p=p.theo)
multinomial.theo.multcomp(counts,p=p.theo)
}