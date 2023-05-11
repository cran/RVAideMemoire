\name{pairwise.mood.medtest}
\alias{pairwise.mood.medtest}
\title{
Pairwise Mood's median tests
}
\description{
Performs pairwise comparisons between group levels with corrections for multiple testing.
}
\usage{
pairwise.mood.medtest(resp, fact, exact = NULL, p.method = "fdr")
}
\arguments{
  \item{resp}{
response vector.
}
  \item{fact}{
grouping factor.
}
  \item{exact}{
a logical indicating whether exact p-values should be computed.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
If \code{exact=NULL}, Fisher's exact tests are used if the number of data values is < 200; otherwise chi-square tests are used (with Yates continuity correction).
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{mood.medtest}}
}
\examples{
set.seed(0904)
response <- c(rnorm(10),rnorm(10,0.8),rnorm(10,2))
fact <- gl(3,10,labels=LETTERS[1:3])
mood.medtest(response~fact)
pairwise.mood.medtest(response,fact)
}