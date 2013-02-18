\name{surv.multcomp}
\alias{surv.multcomp}
\title{
Pairwise comparisons for survival analysis
}
\description{
Perform pairwise comparisons between groups for equality of time to event. The function deals with \code{coxph} and \code{survreg} models.
}
\usage{
surv.multcomp(formula, mat, data, strata = NULL, type = c("coxph", "survreg"), distribution = "exponential", p.method = "fdr")
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} is a \code{Surv()} object and \code{b} is a factor giving levels to be compared.
}
  \item{mat}{
matrix of comparisons (see help of the \code{mat.cont()} function in this package, and the \code{esticon()} function in the package doBy).
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{strata}{
strata, if needed.
}
  \item{type}{
model type: \code{"coxph"} (default) or \code{"survreg"}.
}
  \item{distribution}{
distribution when a \code{"survreg"} model is used.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\details{
When pairwise comparisons are based on a \code{"coxph"} model, a likelihood ratio test is used.
When pairwise comparisons are based on a \code{"survreg"} model, a logrank test is used.
}
\value{
  \item{model}{model formula.}
  \item{statistic}{test statistics.}
  \item{method}{a character string giving the name of the test computed.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value2}{corrected p-values.}
  \item{p.value}{table of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[survival]{survreg}}, \code{\link[survival]{coxph}}
}
\examples{
require(survival)
death <- c(sample(8:15,20,replace=TRUE),sample(12:20,20,replace=TRUE),sample(18:22,20,replace=TRUE))
groups <- gl(3,20,labels=LETTERS[1:3])
model1 <- coxph(Surv(death)~groups)
model1
mat <- matrix(c(1,-1,0,0,1,-1,2,-1,-1),ncol=3,byrow=TRUE,dimnames=list(1:3,levels(groups)))
mat
surv.multcomp(Surv(death)~groups,mat,type="coxph")

model2 <- survreg(Surv(death)~groups,dist="weibull")
model2
surv.multcomp(Surv(death)~groups,mat,type="survreg",distribution="weibull")
}