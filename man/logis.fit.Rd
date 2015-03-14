\name{logis.fit}
\alias{logis.fit}
\title{
Graphical adujstment of a simple binary logistic regression to data
}
\description{
Cuts the data into intervals, compute the response probability and its standard error for each interval and add the results to the regression curve. No test is performed but this permits to have a graphical idea of the adjustment of the model to the data.
}
\usage{
logis.fit(model, int = 5, ...)
}
\arguments{
  \item{model}{
\code{\link{glm}} model.
}
  \item{int}{
number of intervals.
}
  \item{\dots}{
other arguments. See help of \code{\link{points}} and \code{\link{segments}}.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{glm}}
}
\examples{
x <- 1:50
y <- c(rep(0,18),sample(0:1,14,replace=TRUE),rep(1,18))
model <- glm(y~x,family=binomial)
plot(x,y)
lines(x,model$fitted)
logis.fit(model)
}