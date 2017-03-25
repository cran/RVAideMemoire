\name{pairwise.var.test}
\alias{pairwise.var.test}
\title{
Pairwise F tests
}
\description{
Performs pairwise comparisons between group levels with corrections for multiple testing.
}
\usage{
pairwise.var.test(resp, fact, p.method = "fdr",
  alternative = c("two.sided","less", "greater"))
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
\code{\link[RVAideMemoire]{pairwise.perm.var.test}}
}
\examples{
require(graphics)

# Bartlett test
bartlett.test(count~spray,data=InsectSprays)

# Pairwise comparisons
pairwise.var.test(InsectSprays$count,InsectSprays$spray)
}