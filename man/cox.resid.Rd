\name{cox.resid}
\alias{cox.resid}
\title{
Martingale residuals of a Cox model
}
\description{
Plot martingale residuals of a Cox model against fitted values, to check for log-linearity of covariables.
}
\usage{
cox.resid(model)
}
\arguments{
  \item{model}{
\code{coxph} model.
}
}
\references{
Fox, J. 2002 Cox Proportional-Hazards Regression for Survival Data.
}
\author{
Maxime Hervé <mx.herve@gmail.com>, based on a John Fox idea.
}
\seealso{
\code{\link[survival]{coxph}}
}
\examples{
# 'kidney' dataset in MASS package
data(kidney)
model <- coxph(Surv(time,status)~age+factor(sex),data=kidney)
cox.resid(model)
}