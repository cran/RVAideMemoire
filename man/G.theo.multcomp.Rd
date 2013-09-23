\name{G.theo.multcomp}
\alias{G.theo.multcomp}
\title{
Pairwise comparisons after a G-test for given probabilities
}
\description{
Perform pairwise comparisons after a global G-test for given probabilities.
}
\usage{
G.theo.multcomp(x, p = rep(1/length(x), length(x)), p.method = "fdr")
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
\code{\link{G.test}}
}
\examples{
counts <- c(5,15,23,8,14)
p.theo <- c(0.1,0.4,0.3,0.15,0.05)
G.test(counts,p=p.theo)
G.theo.multcomp(counts,p=p.theo)
}