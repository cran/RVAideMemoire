\name{cox.resid}
\alias{cox.resid}
\title{
Martingale residuals of a Cox model
}
\description{
Plots martingale residuals of a Cox model against fitted values, to check for log-linearity of covariates.
}
\usage{
cox.resid(model)
}
\arguments{
  \item{model}{
a \code{\link[survival]{coxph}} model.
}
}
\references{
Fox, J. 2002 Cox Proportional-Hazards Regression for Survival Data.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>, based on an idea of John Fox.
}
\seealso{
\code{\link[survival]{coxph}}
}
\examples{
# 'kidney' dataset of package 'survival'
require(survival)
data(kidney)
model <- coxph(Surv(time,status)~age+factor(sex),data=kidney)
cox.resid(model)
}