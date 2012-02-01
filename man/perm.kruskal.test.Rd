\name{perm.kruskal.test}
\alias{perm.kruskal.test}
\title{
Permutational Kruskal-Wallis rank sum test
}
\description{
Perform a permutational Kruskal-Wallis rank sum test.
}
\usage{
perm.kruskal.test(formula, data=NULL, nperm = 999)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{nperm}{
number of permutations.
}
}
\value{
  \item{statistic}{test statistics of the parametric test.}
  \item{permutations}{number of permutations.}
  \item{p.value}{p-value of the permutational test.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{kruskal.test}}
}
\examples{
response <- c(rnorm(5),rpois(5,1),rnorm(5,5,3))
fact <- factor(rep(LETTERS[1:3],each=5))
perm.kruskal.test(response~fact)
}