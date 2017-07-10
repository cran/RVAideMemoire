\name{pairwise.perm.var.test}
\alias{pairwise.perm.var.test}
\title{
Pairwise permutation F tests
}
\description{
Performs pairwise comparisons between group levels with corrections for multiple testing.
}
\usage{
pairwise.perm.var.test(resp, fact, p.method = "fdr",
  alternative = c("two.sided","less", "greater"), nperm = 999,
  progress = TRUE)
}
\arguments{
  \item{resp}{
response vector.
}
  \item{fact}{
grouping factor.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
}
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
}
\details{
The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{method}{a character string giving the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{permutations}{number of permutations.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{pairwise.var.test}}
}
\examples{
set.seed(0921)
response <- c(rnorm(10),rpois(10,0.2),rnorm(10,,2))
fact <- gl(3,10,labels=LETTERS[1:3])

# Not enough permutations here but it runs faster

# permutation Bartlett test
perm.bartlett.test(response~fact,nperm=49)

# Pairwise comparisons
pairwise.perm.var.test(response,fact,nperm=49)
}