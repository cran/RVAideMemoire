\name{cochran.qtest}
\alias{cochran.qtest}
\title{
Cochran's Q test
}
\description{
Perform the Cochran's Q test for unreplicated randomized block design experiments with a binary response variable and paired data. If the p-value of the test is significant, the function performs pairwise comparisons by using the Wilcoxon sign test.
}
\usage{
cochran.qtest(formula, data, alpha = 0.05, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b | c}, where \code{a}, \code{b} and \code{c} give the data values and corresponding groups and blocks, respectively. \code{a} can be a numeric vector or a factor, with only two possible values.
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
\value{
  \item{method.test}{a character string giving the name of the global test computed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{estimate}{the estimated probabilities.}
  \item{null.value}{the value of the difference in probabilities under the null hypothesis, always 0.}
  \item{statistic}{test statistics (Pearson's Chi-squared test only).}
  \item{parameter}{test degrees of freedom (Pearson's Chi-squared test only).}
  \item{p.value}{p-value of the global test.}
  \item{alpha}{significance level.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value.multcomp}{data frame of pairwise comparisons result.}
  \item{method.multcomp}{a character string giving the name of the test computed for pairwise comparisons.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
fact <- gl(3,1,30,labels=LETTERS[1:3])
block <- gl(10,3,labels=letters[1:10])
cochran.qtest(response~fact|block)
}