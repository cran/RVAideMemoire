\name{cor.multcomp}
\alias{cor.multcomp}
\title{
Comparison of several Pearson's linear correlation coefficients
}
\description{
Perform comparisons of several Pearson's linear correlation coefficients.
If no difference, the function returns the common correlation coefficient, its confidence interval and test for its equality to a given value.
If difference is significative, the functions performs pairwise comparisons between coefficients.
}
\usage{
cor.multcomp(var1, var2, fact, alpha = 0.05, conf.level = 0.95, theo = 0, p.method = "fdr")
}
\arguments{
  \item{var1}{
numeric vector (first variable).
}
  \item{var2}{
numeric vector (second variable).
}
  \item{fact}{
factor (groups).
}
  \item{alpha}{
significance level.
}
  \item{conf.level}{
confidence level.
}
  \item{theo}{
theoretical coefficient.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{method.test}{a character string giving the name of the global test computed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{statistic}{test statistics.}
  \item{parameter}{test degrees of freedom.}
  \item{p.value}{p-value for comparison of the coefficients.}
  \item{null.value}{the value of the difference in coefficients under the null hypothesis, always 0.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{estimate}{the estimated correlation coefficients.}
  \item{alpha}{significance level.}
  \item{conf.level}{confidence level.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value.multcomp}{data frame of pairwise comparisons result.}
  \item{common.name}{a character string explaining the elements of the table below.}
  \item{common}{data frame of results if the coefficients are not significantly different (common coefficient).}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{cor.test}}
}
\examples{
var1 <- c(1:15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
var2 <- c(-1:-15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
fact <- gl(3,15,labels=LETTERS[1:3])
cor.multcomp(var1,var2,fact)

var3 <- c(1:15+rnorm(15,0,1),1:15+rnorm(15,0,3),1:15+rnorm(15,0,2))
cor.multcomp(var1,var3,fact)
}