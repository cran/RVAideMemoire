\name{chisq.theo.multcomp}
\alias{chisq.theo.multcomp}
\title{
Pairwise comparisons after a chi-squared test for given probabilities
}
\description{
Perform pairwise comparisons after a global chi-squared test for given probabilities.
}
\usage{
chisq.theo.multcomp(x, p, p.method = "fdr")
}
\arguments{
  \item{x}{
numeric vector (counts).
}
  \item{p}{
theoretical proportions.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{observed}{observed counts.}
  \item{expected}{expected counts.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{statistic}{statistics of each test.}
  \item{p.value}{corrected p-values.}
  \item{comp}{data frame of results.}
}
\author{
Maxime Herv� <mx.herve@gmail.com>
}
\seealso{
\code{\link{chisq.test}}
}
\examples{
counts <- c(5,15,23,8,14)
p.theo <- c(0.1,0.4,0.3,0.15,0.05)
chisq.test(counts,p=p.theo)
chisq.theo.multcomp(counts,p=p.theo)
}