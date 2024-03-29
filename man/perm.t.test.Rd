\name{perm.t.test}
\alias{perm.t.test}
\alias{perm.t.test.default}
\alias{perm.t.test.formula}
\title{
Permutation Student's t-test
}
\description{
Performs a permutation Student's t-test.
}
\usage{

perm.t.test(x, ...)

\method{perm.t.test}{default}(x, y, paired = FALSE, ...)

\method{perm.t.test}{formula}(formula, data, alternative = c("two.sided", "less", "greater"),
  paired = FALSE, nperm = 999, progress = TRUE, ...)
}
\arguments{
  \item{x}{
a numeric vector of data values.
}
  \item{y}{
a numeric vector of data values.
}
  \item{paired}{
a logical indicating whether you want a paired t-test.
}
  \item{formula}{
a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor with 2 levels giving the corresponding groups.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
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
  \item{\dots}{
further arguments to be passed to or from other methods.
}
}
\details{
The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{statistic}{test statistics of the parametric test.}
  \item{permutations}{number of permutations.}
  \item{p.value}{p-value of the permutation test.}
  \item{estimate}{the estimated mean or difference in means depending on whether it was a paired or not paired test.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating what type of t-test was performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{null.value}{the specified hypothesized value of the mean difference, always 0.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{t.test}}
}
\examples{
response <- c(rnorm(5),rnorm(5,2,1))
fact <- gl(2,5,labels=LETTERS[1:2])

# Not enough permutations here but faster to run

# Unpaired test
perm.t.test(response~fact,nperm=49)

# Paired test
perm.t.test(response~fact,paired=TRUE,nperm=49)
}