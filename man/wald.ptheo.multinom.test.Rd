\name{wald.ptheo.multinom.test}
\alias{wald.ptheo.multinom.test}
\title{
Wald tests for comparison of proportions to theoretical values
}
\description{
Performs pairwise comparisons of proportions to theoretical values.
}
\usage{
wald.ptheo.multinom.test(x, p, p.method = "fdr")
}
\arguments{
  \item{x}{
either a factor or a matrix with K columns giving the counts for each of the K classes.
}
  \item{p}{
theoretical proportions.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
The function builds K logistic regressions (in each case considering one class vs. the sum of all others) and uses \code{\link[RVAideMemoire]{wald.ptheo.test}} to test the hypothesis that the proportion of this class is equal to \code{p[K]}.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{observed proportions.}
  \item{expected}{theoretical proportions.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{statistic}{statistics of each test.}
  \item{p.value2}{corrected p-values.}
  \item{p.value}{data frame of results.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{wald.ptheo.test}}, \code{\link[RVAideMemoire]{prop.multinom}}
}
\examples{
response <- factor(rep(LETTERS[1:4],c(20,40,42,13)))
wald.ptheo.multinom.test(response,p=c(0.15,0.25,0.3,0.3))
}