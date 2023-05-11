\name{chisq.multcomp}
\alias{chisq.multcomp}
\title{
Pairwise comparisons after a chi-squared goodness-of-fit test
}
\description{
Performs pairwise comparisons after a global chi-squared goodness-of-fit test.
}
\usage{
chisq.multcomp(x, p.method = "fdr")
}
\arguments{
  \item{x}{
numeric vector (counts).
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
Since a chi-squared test is an approximate test, an exact test is preferable when the number of individuals is small (200 is a reasonable minimum). See \code{\link[RVAideMemoire]{multinomial.multcomp}} in that case.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{chisq.test}}, \code{\link[RVAideMemoire]{multinomial.test}}, \code{\link[RVAideMemoire]{multinomial.multcomp}}
}
\examples{
counts <- c(49,30,63,59)
chisq.test(counts)
chisq.multcomp(counts)
}