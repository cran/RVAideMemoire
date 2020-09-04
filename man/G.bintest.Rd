\name{G.bintest}
\alias{G.bintest}
\title{
G-test for binary variables
}
\description{
Performs a G-test for comparing response probabilities (i.e. when the response variable is a binary variable). The function is in fact a wrapper to the G-test for comparison of proportions on a contingency table. If the p-value of the test is significant, the function performs pairwise comparisons by using G-tests.
}
\usage{
G.bintest(formula, data, alpha = 0.05, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} and \code{b} give the data values and corresponding groups, respectively. \code{a} can be a numeric vector or a factor, with only two possible values (except NA).
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{alpha}{
significance level to compute pairwise comparisons.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
If the response is a 0/1 variable, the probability of the '1' group is tested. In any other cases, the response is transformed into a factor and the probability of the second level is tested.

Since a G-test is an approximate test, an exact test is preferable when the number of individuals is small (200 is a reasonable minimum). See \code{\link[RVAideMemoire]{fisher.bintest}} in that case.
}
\value{
  \item{method.test}{a character string giving the name of the global test computed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{estimate}{the estimated probabilities.}
  \item{null.value}{the value of the difference in probabilities under the null hypothesis, always 0.}
  \item{statistic}{test statistics.}
  \item{parameter}{test degrees of freedom.}
  \item{p.value}{p-value of the global test.}
  \item{alpha}{significance level.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value.multcomp}{data frame of pairwise comparisons result.}
  \item{method.multcomp}{a character string giving the name of the test computed for pairwise comparisons.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{chisq.bintest}}, \code{\link[RVAideMemoire]{fisher.bintest}}
}
\examples{
response <- c(rep(0:1,c(40,60)),rep(0:1,c(55,45)),rep(0:1,c(65,35)))
fact <- gl(3,100,labels=LETTERS[1:3])
G.bintest(response~fact)
}