\name{perm.bartlett.test}
\alias{perm.bartlett.test}
\title{
Permutational Bartlett's test of homogeneity of variances
}
\description{
Performs a permutational Bartlett's test of homogeneity of k variances.
}
\usage{
perm.bartlett.test(formula, data, nperm = 999, progress = TRUE)
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
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
}
\details{
The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{statistic}{test statistics of the parametric test.}
  \item{permutations}{number of permutations.}
  \item{p.value}{p-value of the permutational test.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{bartlett.test}}
}
\examples{
response <- c(rnorm(12),rpois(12,1),rnorm(12,2,1))
fact <- gl(3,12,labels=LETTERS[1:3])
perm.bartlett.test(response~fact)
}