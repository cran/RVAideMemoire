\name{pairwise.factorfit}
\alias{pairwise.factorfit}
\title{
Pairwise comparisons of groups displayed on a factorial map
}
\description{
Performs pairwise comparisons between group levels with corrections for multiple testing. Tests are computed using \code{\link[vegan:envfit]{factorfit}}.
}
\usage{
pairwise.factorfit(ord, fact, xax = 1, yax = 2, nperm = 999,
  p.method = "fdr", ...)
}
\arguments{
  \item{ord}{
any multivariate analysis handled by \code{\link[RVAideMemoire]{MVA.scores}}.
}
  \item{fact}{
grouping factor.
}
  \item{xax}{
first axis of the factorial map.
}
  \item{yax}{
second axis of the factorial map.
}
  \item{nperm}{
number of permutations.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{\dots}{
optional further agruments to \code{\link[RVAideMemoire]{MVA.scores}}.
}
}
\value{
  \item{method}{a character string giving the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data and the number of permutations.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[vegan:envfit]{factorfit}}
}
\examples{
require(vegan)
data(iris)

PCA <- rda(iris[,1:4])
MVA.plot(PCA,fac=iris$Species,col=1:3)

# Global test
envfit(PCA~Species,data=iris)

# Pairwise comparisons
# (not enough permutations here but faster to run)
pairwise.factorfit(PCA,iris$Species,nperm=49)
}