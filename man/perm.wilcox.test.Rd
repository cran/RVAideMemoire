\name{perm.wilcox.test}
\alias{perm.wilcox.test}
\title{
Permutational Wilcoxon rank sum and signed rank tests
}
\description{
Perform a permutational Wilcoxon rank sum test (if data are unpaired) or a Wilcoxon signed rank test (if data are paired).
}
\usage{
perm.wilcox.test(formula, data=NULL, alternative = c("two.sided", "less", "greater"), paired = FALSE, nperm = 999)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor with 2 levels giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{alternative}{
a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
}
\item{paired}{
a logical indicating whether you want a paired test.
}
  \item{nperm}{
number of permutations.
}
}
\value{
  \item{statistic}{test statistics of the parametric test.}
  \item{permutations}{number of permutations.}
  \item{p.value}{p-value of the permutational test.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{H1}{a character string giving the explicit alternative hypothesis.}
  \item{method}{a character string indicating what type of test was performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{wilcox.test}}
}
\examples{
response <- c(rnorm(5),rpois(5,4))
fact <- factor(rep(LETTERS[1:2],each=5))

# Unpaired test
perm.wilcox.test(response~fact)

# Paired test
perm.wilcox.test(response~fact,paired=TRUE)
}