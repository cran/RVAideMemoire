\name{pairwise.perm.manova}
\alias{pairwise.perm.manova}
\title{
Pairwise permutational MANOVAs
}
\description{
Performs pairwise comparisons between group levels with corrections for multiple testing. These pairwise comparisons are relevant after a permutational MANOVA, such as performed by \code{\link[vegan]{adonis}}.
}
\usage{
pairwise.perm.manova(resp, fact, test = c("Pillai", "Wilks",
  "Hotelling-Lawley", "Roy", "Spherical"), nperm = 999, 
  progress = TRUE, p.method = "fdr")
}
\arguments{
  \item{resp}{
response. Either a matrix (one column per variable; objects of class \code{"data.frame"} are accepted and internally converted into matrices) or a distance matrix.
}
  \item{fact}{
grouping factor.
}
  \item{test}{
choice of test statistic when \code{resp} is a matrix (see \code{\link{anova.mlm}}).
}
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
If \code{resp} is a matrix, a classical MANOVA is performed and the distribution of the (pseudo-)F is computed through permutations. The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.

If \code{resp} is a distance matrix, \code{\link[vegan]{adonis}} is used to perform each comparison.
}
\value{
  \item{method}{a character string giving the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data and the number of permutations.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{anova.mlm}}, \code{\link[vegan]{adonis}}
}
\examples{
require(vegan)
data(iris)

# Permutational MANOVA
adonis(iris[,1:4]~Species,data=iris,method="euclidian")

# Pairwise comparisons
# (not enough permutations here but faster to run)
pairwise.perm.manova(iris[,1:4],iris$Species,nperm=49)

# or
pairwise.perm.manova(dist(iris[,1:4],"euclidian"),iris$Species,nperm=49)
}