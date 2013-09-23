\name{predict.least.rect}
\alias{predict.least.rect}
\title{
Predict method for least rectangles linear regression
}
\description{
Predicted values based on a least rectangles linera regression fitted by \code{\link[RVAideMemoire]{least.rect}}.
}
\usage{
\method{predict}{least.rect}(object, newdata, ...)
}
\arguments{
  \item{object}{
regression fitted by \code{\link[RVAideMemoire]{least.rect}}.
}
  \item{newdata}{
a list or a data frame containing variables with which to predict.
}
  \item{...}{
further arguments.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{least.rect}}
}
\examples{
x <- c(1:30,1:30)
y <- c(1:30+rnorm(30,0,3),seq(10,22,12/29)+rnorm(30,0,3))
fact <- gl(2,30,labels=LETTERS[1:2])
regression <- least.rect(y~x|fact)
predict(regression,newdata=list(x=c(-2,18),fact=c("B","A")))
tab <- data.frame(x=c(-2,18),fact=c("B","A"))
predict(regression,newdata=tab)
}
