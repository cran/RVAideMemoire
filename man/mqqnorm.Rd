\name{mqqnorm}
\alias{mqqnorm}
\title{
Multivariate normality QQ-Plot
}
\description{
Draws a QQ-plot to assess multivariate normality.
}
\usage{
mqqnorm(x, main = "Multi-normal Q-Q Plot")
}
\arguments{
  \item{x}{
a data frame or a matrix of numeric variables (each column giving a variable).
}
  \item{main}{
title of the graph.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{mshapiro.test}}, \code{\link[car]{qqPlot}}
}
\examples{
x <- 1:30+rnorm(30)
y <- 1:30+rnorm(30,1,3)
mqqnorm(cbind(x,y))
}
