\name{pairwise.perm.t.test}
\alias{pairwise.perm.t.test}
\title{
Pairwise permutational t tests
}
\description{
Perform pairwise comparisons between group levels with corrections for multiple testing.
}
\usage{
pairwise.perm.t.test(resp, fact, p.method = "fdr", paired = FALSE, alternative = c("two.sided","less", "greater"), nperm = 999)
}
\arguments{
  \item{resp}{
response vector.
}
  \item{fact}{
grouping factor.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
  \item{paired}{
a logical indicating whether you want paired (permutational) t-tests.
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
}
  \item{nperm}{
number of permutations.
}
}
\value{
  \item{method}{a character string indicating what type of t-tests were performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{permutations}{number of permutations.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{pairwise.t.test}}
}
\examples{
set.seed(1203)
response <- c(rnorm(5),rpois(5,0.5),rnorm(5,2,1))
fact <- gl(3,5,labels=LETTERS[1:3])

# Permutational ANOVA
perm.anova(response~fact)

# Pairwise comparisons
pairwise.perm.t.test(response,fact)
}