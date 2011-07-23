\name{cor.2comp}
\alias{cor.2comp}
\title{
Comparison of 2 Pearson's linear correlation coefficients
}
\description{
Perform the test for equality of 2 Pearson's correlation coefficients. If difference is not significative, the function returns the common coefficient, its confidence interval and performs the test for equality to a given value.
}
\usage{
cor.2comp(var1, var2, var3, var4, alpha = 0.05, conf.level = 0.95, theo = 0)
}
\arguments{
  \item{var1}{
numeric vector (first variable of the first correlation).
}
  \item{var2}{
numeric vector (second variable of the first correlation).
}
  \item{var3}{
numeric vector (first variable of the second correlation).
}
  \item{var4}{
numeric vector (second variable of the second correlation).
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
}
\value{
  \item{conf.level}{confidence level.}
  \item{alpha}{significance level.}
  \item{coeffs}{observed Pearson's coefficients.}
  \item{u.comp}{test statistics for comparison of the 2 coefficients.}
  \item{p.com}{p-value for comparison of the 2 coefficients.}
  \item{comp}{data.frame of results for comparison or the 2 coefficients.}
  \item{r.comm}{common correlation coefficient.}
  \item{r.theo}{theoretical coefficient.}
  \item{u.comm}{test statistics for equality of the common coefficient to the theoretical value.}
  \item{p.comm}{p-value for equality of the common coefficient to the theoretical value.}
  \item{comm}{data.frame or results for equality of the common coefficient to the theoretical value.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{cor.test}}
}
\examples{
cor1.var1<-1:30+rnorm(30,0,2)
cor1.var2<-1:30+rnorm(30,0,3)
cor2.var1<-(-1):-30+rnorm(30,0,2)
cor2.var2<-(-1):-30+rnorm(30,0,3)
cor.2comp(cor1.var1,cor1.var2,cor2.var1,cor2.var2)
}