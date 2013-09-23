\name{cor.2comp}
\alias{cor.2comp}
\title{
Comparison of 2 Pearson's linear correlation coefficients
}
\description{
Perform the test for equality of 2 Pearson's correlation coefficients. If the difference is not significative, the function returns the common coefficient, its confidence interval and performs the test for equality to a given value.
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
  \item{method.test}{a character string giving the name of the global test computed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{statistic}{test statistics.}
  \item{p.value}{p-value for comparison of the 2 coefficients.}
  \item{null.value}{the value of the difference in coefficients under the null hypothesis, always 0.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{estimate}{the estimated correlation coefficients.}
  \item{alpha}{significance level.}
  \item{conf.level}{confidence level.}
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
cor1.var1 <- 1:30+rnorm(30,0,2)
cor1.var2 <- 1:30+rnorm(30,0,3)
cor2.var1 <- (-1):-30+rnorm(30,0,2)
cor2.var2 <- (-1):-30+rnorm(30,0,3)
cor.2comp(cor1.var1,cor1.var2,cor2.var1,cor2.var2)
}