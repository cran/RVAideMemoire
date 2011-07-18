\name{wilcox.paired.multcomp}
\alias{wilcox.paired.multcomp}
\title{
Pairwise Wilcoxon signed rank sum test
}
\description{
Perform pairwise comparisons by Wilcoxon signed rank sum test for paired data.
}
\usage{
wilcox.paired.multcomp(resp, fact, block, p.method = "fdr")
}
\arguments{
  \item{resp}{
response variable (numeric vector).
}
  \item{fact}{
factor.
}
  \item{block}{
factor for blocs of paired data (random factor).
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{datas}{variables name.}
  \item{p.method}{method for p-values correction.}
  \item{V}{test statistics.}
  \item{p.value}{corrected p-values.}
  \item{comp}{data.frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{pairwise.wilcox.test}}
}
\examples{
response<-c(rnorm(10,0,3),rnorm(10,5,3),rnorm(10,8,2))
factor<-factor(rep(LETTERS[1:3],each=10))
block<-factor(rep(letters[1:10],3))
friedman.test(response~factor|block)
wilcox.paired.multcomp(response,factor,block)
}