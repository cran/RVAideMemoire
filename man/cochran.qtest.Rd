\name{cochran.qtest}
\alias{cochran.qtest}
\title{
Cochran Q test
}
\description{
Perform the Cochran Q test for unreplicated randomized block design experiments whith a binary response variable and paired data. If the p-value of the test is significative, the function performs pairwise comparisons using Wilcoxon sign test.
}
\usage{
cochran.qtest(formula, data=NULL, alpha = 0.05, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b | c}, where \code{a}, \code{b} and \code{c} give the data values and corresponding groups and blocks, respectively.
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
  \item{data}{variables name.}
  \item{alpha}{significance level.}
  \item{Q}{test statistics.}
  \item{p.value}{p-value of the global test.}
  \item{tab.test}{data frame of global test result.}
  \item{p.method}{method for p-values correction.}
  \item{p.multcomp}{corrected p-values.}
  \item{multcomp}{data frame of pairwise comparisons result.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
response<-c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
factor<-factor(rep(LETTERS[1:3],10))
block<-factor(rep(letters[1:10],each=3))
cochran.qtest(response~factor|block)
}