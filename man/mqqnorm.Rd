\name{mqqnorm}
\alias{mqqnorm}
\title{
Multivariate normality QQ-Plot
}
\description{
Draw a QQ-plot to assess multivariate normality.
}
\usage{
mqqnorm(x)
}
\arguments{
  \item{x}{
a numeric matrix of data values (each line giving a variable).
}
}
\author{
Maxime Herv� <mx.herve@gmail.com>
}
\seealso{
\code{\link[mvnormtest]{mshapiro.test}}
}
\examples{
x <- 1:30+rnorm(30)
y <- 1:30+rnorm(30,1,3)
mqqnorm(rbind(x,y))
}