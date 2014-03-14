\name{wilcox.paired.rating.multcomp}
\alias{wilcox.paired.rating.multcomp}
\title{
Non parametric pairwise comparisons for paired ratings
}
\description{
Perform non parametric pairwise comparisons of paired ratings (ordinal response variables) by Wilcoxon signed rank tests for paired data.
}
\usage{
wilcox.paired.rating.multcomp(formula, data, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b | c}, where \code{a}, \code{b} and \code{c} give the data values and corresponding groups and blocks, respectively. It is preferable that \code{a} is an ordered factor.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{method}{a character string indicating the name of the test.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results of pairwise comparisons.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{pairwise.wilcox.rating.test}}, \code{\link[RVAideMemoire]{wilcox.rating.test}}
}
\examples{
set.seed(1630)
response <- factor(c(sample(1:3,10,replace=TRUE),
  sample(2:5,10,replace=TRUE),
  sample(3:6,10,replace=TRUE)),levels=c("1","2","3","4","5","6"),ordered=TRUE)
fact <- gl(3,10,labels=LETTERS[1:3])
block <- gl(10,1,30,labels=letters[1:10])
friedman.rating.test(response~fact|block)
wilcox.paired.rating.multcomp(response~fact|block)
}