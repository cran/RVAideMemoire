\name{logis.noise}
\alias{logis.noise}
\title{
Creating a nls model for logistic regression from fitted values of a glm model
}
\description{
Add some noise to the fitted values of a \code{glm} model to create a \code{nls} model for logistic regression (creating a \code{nls} model from exact fitted values does not work).
}
\usage{
logis.noise(model, intensity = 25)
}
\arguments{
  \item{model}{
\code{glm} model.
}
  \item{intensity}{
intensity of the noise: lower the value, bigger the noise.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{glm}}, \code{\link{nls}}
}
\examples{
x<-1:50
y<-c(rep(0,18),sample(0:1,14,replace=TRUE),rep(1,18))
model<-glm(y~x,family=binomial)
y2<-logis.noise(model)
# Then model2<-nls(y2~SSlogis(...))
}