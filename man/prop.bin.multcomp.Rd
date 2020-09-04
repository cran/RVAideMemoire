\name{prop.bin.multcomp}
\alias{prop.bin.multcomp}
\title{
Pairwise comparisons after a test for given probabilities
}
\description{
Performs pairwise comparisons after a global test for given response probabilities (i.e. when the response variable is a binary variable), by using exact binomial tests. The function is in fact a wrapper to pairwise comparisons of proportions to given values on a contingency table.
}
\usage{
prop.bin.multcomp(formula, data, p, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} and \code{b} give the data values and corresponding groups, respectively. \code{a} can be a numeric vector or a factor, with only two possible values (except NA).
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{p}{
theoretical probabilities.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
If the response is a 0/1 variable, the probability of the '1' group is tested. In any other cases, the response is transformed into a factor and the probability of the second level is tested.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{observed}{observed probabilities.}
  \item{expected}{expected probabilities.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value2}{corrected p-values.}
  \item{p.value}{table or results of pairwise comparisons.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{prop.multcomp}}, \code{\link[RVAideMemoire]{chisq.theo.bintest}}
}
\examples{
response <- c(rep(0:1,c(40,60)),rep(0:1,c(55,45)),rep(0:1,c(65,35)))
fact <- gl(3,100,labels=LETTERS[1:3])
p.theo <- c(0.5,0.45,0.2)
chisq.theo.bintest(response~fact,p=p.theo)
prop.bin.multcomp(response~fact,p=p.theo)
}