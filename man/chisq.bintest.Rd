\name{chisq.bintest}
\alias{chisq.bintest}
\title{
Pearson's Chi-squared test or Fisher's exact test for binary variables
}
\description{
Perform a Pearson's Chi-squared test for comparing response probabilities (i.e. when the response variable is a binary variable), or a Fisher's exact test for count data if more than 20\% of expected counts are <5 (Cochran's rule). The function is in fact a wrapper to the chi-squared test for comparison of proportions on a contingency table. If the p-value of the test is significative, the function performs pairwise comparisons, by using Pearson's Chi-squared test or Fisher's exact test depending on the respect to Cochran's rule.
}
\usage{
chisq.bintest(formula, data=NULL, alpha = 0.05, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} and \code{b} give the data values and corresponding groups, respectively.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{alpha}{
significance level to compute pairwise comparisons.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{alpha}{significance level.}
  \item{test}{a short character string giving the test computed.}
  \item{statistic}{test statistics (Pearson's Chi-squared test only).}
  \item{parameter}{test degrees of freedom (Pearson's Chi-squared test only).}
  \item{p.value}{p-value of the global test.}
  \item{method}{a long character string giving the test computed.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{multcomp}{data frame of pairwise comparisons result.}
  \item{multcomp.method}{a character string giving the test computed for pairwise comparisons.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
response <- c(0,0,0,0,0,0,1,0,0,1,0,0,1,0,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1)
fact <- gl(3,10,labels=LETTERS[1:3])
chisq.bintest(response~fact)
}