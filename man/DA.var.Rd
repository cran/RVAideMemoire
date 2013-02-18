\name{DA.var}
\alias{DA.var}
\title{
Explained variance in Linear Discriminant Analysis
}
\description{
Calculate intergroup variance explained by components of a Linear Discriminant Analysis (obtained from the function \code{lda)} of the package MASS).
}
\usage{
DA.var(model)
}
\arguments{
  \item{model}{
object of class \code{"lda"} (from the function \code{lda()} of the package MASS).
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[MASS]{lda}}
}
\examples{
# LDA model
require(MASS)
data(iris)
model.LDA <- lda(iris[,1:4],iris$Species)
DA.var(model.LDA)
}
