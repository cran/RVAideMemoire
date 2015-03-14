\name{pcor.test}
\alias{pcor.test}
\title{
Tests for (semi-)partial association/correlation between paired samples
}
\description{
Test for (semi-)partial association between paired samples while controlling for other variables, using one of Pearson's product moment correlation coefficient or Spearman's \emph{rho}.
}
\usage{
pcor.test(x, y, z, semi = FALSE, conf.level = 0.95, nrep = 1000,
  method = c("pearson", "spearman"))
}
\arguments{
  \item{x}{
a numeric vector.
}
  \item{y}{
a numeric vector.
}
  \item{z}{
a numeric vector, matrix, data frame or list giving the controlling variables. For matrices, variables must be placed in columns.
}
  \item{semi}{
logical. If \code{TRUE} the semi-partial correlation coefficient is computed and tested. In that case only \code{y} is controlled for \code{z}.
}
  \item{conf.level}{
confidence level for confidence interval..
}
  \item{nrep}{
number of replicates for computation of the confidence interval of a Spearman's rank correlation coefficient (by bootstraping).
}
  \item{method}{
a character string indicating which correlation coefficient is to be used for the test. One of "pearson" or "spearman".
}
}
\details{
If \code{method} is \code{"pearson"} and if there are at least 4+\emph{k} complete series of observation (where \emph{k} is the number of controlling variables), an asymptotic confidence interval of the correlation coefficient is given based on Fisher's Z transform.

If \code{method} is \code{"spearman"}, the p-value is computed through the AS89 algorithm if the number of complete series of observation is less than 10, otherwise via the asymptotic \emph{t} approximation (in both cases the \code{\link[pspearman]{pspearman}} function is used). A confidence interval of the correlation coefficient, computed by bootstraping, is given.
}
\value{
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{alternative}{a character string describing the alternative hypothesis, always two-sided.}
  \item{method}{a character string indicating how the association was measured.}
  \item{conf.int}{a condidence interval for the measure of association.}
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{the degrees of freedom of the test (only for a Pearson's correlation coefficient).}
  \item{p.value}{the p-value of the test.}
  \item{estimate}{the estimated measure of association, with name \code{"cor"} or \code{"rho"} corresponding to the method employed.}
  \item{null.value}{he value of the association measure under the null hypothesis, always 0.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{pcor}}
}
\examples{
set.seed(1444)
x <- 1:30
y <- 1:30+rnorm(30,0,2)
z1 <- runif(30,0,4)
z2 <- 30:1+rnorm(30,03)
pcor.test(x,y,z1)
pcor.test(x,y,list(z1,z2))
}