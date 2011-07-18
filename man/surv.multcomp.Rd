\name{surv.multcomp}
\alias{surv.multcomp}
\title{
Pairwise comparisons for survival analysis.
}
\description{
Perform pairwise comparisons between groups for equality of time to event. The function deals with coxph and survreg models.
}
\usage{
surv.multcomp(surv, fact, mat, strata = NULL, type = c("survreg", "coxph"), distribution = "exponential", p.method = "fdr")
}
\arguments{
  \item{surv}{
survival object. See help of the \code{Surv()} function.
}
  \item{fact}{
factor (groups).
}
  \item{mat}{
matrix of comparisons (see help of the \code{esticon()} function in doBy package).
}
  \item{strata}{
strata, if needed.
}
  \item{type}{
model type: \code{"survreg"} or \code{"coxph"}.
}
  \item{distribution}{
distribution when a survreg model is used.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{model}{model formula.}
  \item{statistics}{test statistics.}
  \item{p.method}{method for p-values correction.}
  \item{p.value}{corrected p-values.}
  \item{comparisons}{data.frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[survival]{survreg}}, \code{\link[survival]{coxph}}
}
\examples{
death<-c(sample(8:15,20,replace=TRUE),sample(12:20,20,replace=TRUE),sample(18:22,20,replace=TRUE))
groups<-factor(rep(LETTERS[1:3],each=20))
model1<-coxph(Surv(death)~groups)
model1
mat<-matrix(c(1,-1,0,0,1,-1,2,-1,-1),ncol=3,byrow=TRUE,dimnames=list(1:3,levels(groups)))
surv.multcomp(Surv(death),groups,mat,type="coxph")

model2<-survreg(Surv(death)~groups,dist="weibull")
model2
surv.multcomp(Surv(death),groups,mat,type="survreg",distribution="weibull")
}