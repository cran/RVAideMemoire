\name{cox.resid}
\alias{cox.resid}
\title{
Martingale residuals of Cox model
}
\description{
Plot martingale residuals of a Cox model against fitted values to check for log-linearity of covariables.
}
\usage{
cox.resid(model, covar)
}
\arguments{
  \item{model}{
\code{coxph} model.
}
  \item{covar}{
list of covariables.
}
}
\references{
Fox, J. 2002 Cox Proportional-Hazards Regression for Survival Data.
}
\author{
Maxime Herv� <mx.herve@gmail.com>, based on a John Fox idea.
}
\seealso{
\code{\link[survival]{coxph}}
}
\examples{
# 'kidney' dataset in MASS package
data(kidney)
model<-coxph(Surv(time,status)~age+frail,data=kidney)
cox.resid(model,list(age=kidney$age,frail=kidney$frail))
}