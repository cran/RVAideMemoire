\name{perm.var.test}
\alias{perm.var.test}
\title{
Permutational F test to compare two variances
}
\description{
Perform a permutational F test to compare two variances.
}
\usage{
perm.var.test(formula, data=NULL, ratio = 1, alternative = c("two.sided", "less", "greater"), nperm = 999)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor with 2 levels giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{ratio}{
the hypothesized ratio of the two population variances.
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
}
  \item{nperm}{
number of permutations.
}
}
\value{
  \item{statistic}{test statistics of the parametric test.}
  \item{permutations}{number of permutations.}
  \item{p.value}{p-value of the permutational test.}
  \item{estimate}{the ratio of the two variances.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{H1}{a character string giving the explicit alternative hypothesis.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{var.test}}
}
\examples{
response <- c(rpois(8,1),rpois(8,3))
fact <- factor(rep(LETTERS[1:2],each=8))
perm.var.test(response~fact)
}