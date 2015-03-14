\name{chisq.theo.multcomp}
\alias{chisq.theo.multcomp}
\title{
Pairwise comparisons after a chi-squared test for given probabilities
}
\description{
Performs pairwise comparisons after a global chi-squared test for given probabilities.
}
\usage{
chisq.theo.multcomp(x, p = rep(1/length(x), length(x)), p.method = "fdr")
}
\arguments{
  \item{x}{
numeric vector (counts).
}
  \item{p}{
theoretical proportions.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
Since a chi-squared test is an approximate test, an exact test is preferable when the number of individuals is small (200 is a reasonable minimum). See \code{\link[RVAideMemoire]{multinomial.theo.multcomp}} in that case.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{observed counts.}
  \item{expected}{expected counts.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{statistic}{statistics of each test.}
  \item{p.value2}{corrected p-values.}
  \item{p.value}{data frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{chisq.test}}, \code{\link[RVAideMemoire]{multinomial.test}}, \code{\link[RVAideMemoire]{multinomial.theo.multcomp}}
}
\examples{
counts <- c(49,30,63,59)
p.theo <- c(0.2,0.1,0.45,0.25)
chisq.test(counts,p=p.theo)
chisq.theo.multcomp(counts,p=p.theo)
}