\name{chisq.theo.bintest}
\alias{chisq.theo.bintest}
\title{
Pearson's Chi-squared test for comparison of response probabilities to given values
}
\description{
Performs a Pearson's Chi-squared test for comparing response probabilities (i.e. when the response variable is a binary variable) to given values. The function is in fact a wrapper to the chi-squared test for comparison of proportions to given values on a contingency table.
}
\usage{
chisq.theo.bintest(formula, data, p)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} and \code{b} give the data values and corresponding groups, respectively. \code{a} can be a numeric vector or a factor, with only two possible values (except NA).
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{p}{
theoretical probabilities.
}
}
\details{
If the response is a 0/1 variable, the probability of the '1' group is tested. In any other cases, the response is transformed into a factor and the probability of the second level is tested.
}
\value{
  \item{method.test}{a character string giving the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{alternative}{a character string describing the alternative hypothesis, always two-sided.}
  \item{estimate}{the estimated probabilities.}
  \item{null.value}{the theoretical probabilities.}
  \item{statistic}{test statistics.}
  \item{parameter}{test degrees of freedom.}
  \item{p.value}{p-value of the test.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{prop.test}}, \code{\link[RVAideMemoire]{chisq.bin.exp}}, \code{\link[RVAideMemoire]{prop.bin.multcomp}}
}
\examples{
response <- c(rep(0:1,c(40,60)),rep(0:1,c(55,45)),rep(0:1,c(65,35)))
fact <- gl(3,100,labels=LETTERS[1:3])
p.theo <- c(0.5,0.45,0.2)
chisq.theo.bintest(response~fact,p=p.theo)
}