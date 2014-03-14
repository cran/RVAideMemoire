\name{pairwise.G.test}
\alias{pairwise.G.test}
\title{
Pairwise comparisons for proportions using G-tests
}
\description{
Perform pairwise comparisons between pairs of proportions with correction for multiple testing.
}
\usage{
pairwise.G.test(x, p.method = "fdr")
}
\arguments{
 \item{x}{
matrix with 2 columns giving the counts of successes and failures, respectively.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
}
\seealso{
\code{\link{G.test}}
}
\examples{
x <- matrix(c(22,28,13,37,35,15),ncol=2,dimnames=list(c("Control","Treatment1","Treatment2"),
  c("Alive","Dead")),byrow=TRUE)
G.test(x)
pairwise.G.test(x)
}
