\name{DA.var}
\alias{DA.var}
\title{
Explained variance in Discriminant Analysis
}
\description{
Calculate intergroup variance explained by components of a Discriminant Analysis (either obtained from the function \code{lda)} of the package MASS or the function \code{plsda()} of the package mixOmics).
}
\usage{
DA.var(model)
}
\arguments{
  \item{model}{
object of class \code{"lda"} (from the function \code{lda()} of the package MASS) or \code{"plsda"} (from the function \code{plsda()} of the package mixOmics).
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[MASS]{lda}}, \code{\link[mixOmics]{plsda}}
}
\examples{
# LDA model
require(MASS)
data(iris)
model.LDA <- lda(iris[,1:4],iris$Species)
DA.var(model.LDA)

# PLS-DA model
require(mixOmics)
data(yeast)
model.PLSDA <- plsda(t(yeast$data),yeast$strain.cond,ncomp=3)
DA.var(model.PLSDA)
}
