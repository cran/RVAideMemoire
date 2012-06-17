\name{wilcox.paired.multcomp}
\alias{wilcox.paired.multcomp}
\title{
Non parametric pairwise comparisons for paired data
}
\description{
Perform non parametric pairwise comparisons of paired samples by Wilcoxon signed rank tests for paired data.
}
\usage{
wilcox.paired.multcomp(formula, data=NULL, p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b | c}, where \code{a}, \code{b} and \code{c} give the data values and corresponding groups and blocks, respectively.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{method}{a character string indicating the name of the test.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{comp}{table of results of pairwise comparisons.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{pairwise.wilcox.test}}, \code{\link{wilcox.test}}
}
\examples{
response <- c(rnorm(10,0,3),rnorm(10,5,3),rnorm(10,8,2))
fact <- gl(3,10,labels=LETTERS[1:3])
block <- gl(10,1,30,labels=letters[1:10])
friedman.test(response~fact|block)
wilcox.paired.multcomp(response~fact|block)
}