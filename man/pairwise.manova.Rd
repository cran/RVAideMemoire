\name{pairwise.manova}
\alias{pairwise.manova}
\title{
Pairwise MANOVAs
}
\description{
Perform pairwise comparisons between group levels with corrections for multiple testing.
}
\usage{
pairwise.manova(resp, fact, p.method = "fdr")
}
\arguments{
  \item{resp}{
response matrix (one column per variable). Object of class \code{"data.frame"} are accepted and internally converted into matrices.
}
  \item{fact}{
grouping factor.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{method}{a character string giving the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{manova}}, \code{\link{summary.manova}}
}
\examples{
data(iris)

# MANOVA
summary(manova(as.matrix(iris[,1:4])~iris$Species))

# Pairwise comparisons
pairwise.manova(iris[,1:4],iris$Species)
}