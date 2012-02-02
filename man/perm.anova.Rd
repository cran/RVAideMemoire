\name{perm.anova}
\alias{perm.anova}
\alias{perm.anova.1way}
\alias{perm.anova.2wayA}
\alias{perm.anova.2wayB}
\alias{perm.anova.2wayC}
\alias{perm.anova.2wayD}
\title{
Permutational Analysis of Variance
}
\description{
Perform a permutational analysis of variance for 1 or 2 factors. For 2 factors, experiment design must be balanced. The 2 factors can be crossed with or without interaction, or nested. The second factor can be a random factor defining blocks.
}
\usage{
perm.anova(formula, nest.f2 = c("fixed", "random"), data=NULL, nperm = 999)
}
\arguments{
  \item{formula}{
a formula of the form \code{response ~ factor(s)} (see Details).
}
  \item{nest.f2}{
in case of 2 nested factors, precision is needed if the nested factor (factor2) is \code{"fixed"} (default) or \code{"random"}.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{nperm}{
number of permutations.
}
}
\details{
For 2 factors, the formula can be \code{response ~ factor1 + factor2} for 2 fixed factors without interaction; \code{response ~ factor1 * factor2} for 2 fixed factors with interaction; \code{response ~ factor1 / factor2} for 2 fixed factors with factor2 nested into factor1 (if factor2 is a random factor, argument \code{nest.f2} must be changed from \code{"fixed"} (default) to \code{"random"}); \code{response ~ factor1 | factor 2} for one fixed factor (factor1) and one random factor definig blocks (factor2).
}
\value{
  \item{permutations}{number of permutations.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{table}{analysis of variance table.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
set.seed(1203)
response <- c(rnorm(12),rpois(12,0.5),rnorm(12,2,1))

# 1 factor
fact1 <- factor(rep(LETTERS[1:3],each=12))
perm.anova(response~fact1)

# 2 crossed fixed factors without interaction
fact2 <- factor(rep(letters[1:3],12))
perm.anova(response~fact1+fact2)

# 2 crossed fixed factors with interaction
perm.anova(response~fact1*fact2)

# 2 nested fixed factors
fact3 <- factor(rep(letters[1:6],each=6))
perm.anova(response~fact1/fact2)

# 1 fixed factor and 1 random factor (blocks)
block <- factor(rep(rep(letters[1:3],each=4),3))
perm.anova(response~fact1|fact2)
}