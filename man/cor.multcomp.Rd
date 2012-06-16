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
  \item{levels}{number of levels of \code{fact}.}
  \item{conf.level}{confidence level.}
  \item{alpha}{significance level.}
  \item{coeffs}{observed correlation coefficients.}
  \item{statistic.comp}{test statistics for equality of all coefficients.}
  \item{p.value.comp}{p-value for equality of all coefficients.}
  \item{comp}{data frame of results for equality of all coefficients.}
  \item{r.comm}{common correlation coefficient and confidence interval.}
  \item{r.theo}{theoretical coefficient.}
  \item{statistic.comm}{test statistics for equality of the common coefficient to the theoretical value.}
  \item{p.value.comm}{p-value for equality of the common coefficient to the theoretical value.}
  \item{comm}{data frame for equality of the common coefficient to the theoretical value.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.multcomp}{data frame of results of pairwise comparisons.}
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