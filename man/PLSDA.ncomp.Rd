\name{PLSDA.ncomp}
\alias{PLSDA.ncomp}
\title{
Graphical help to number of components selection in PLS-DA
}
\description{
Draws a graph with classification error rate (obtained from cross-validation with the M-fold method) and proportion of intergroup variance explained from one to any possible components included in a given PLS-DA. This helps to choose the number of components to be kept in the model.
}
\usage{
PLSDA.ncomp(X, Y, pred.method = c("mahalanobis.dist", "centroids.dist", "max.dist"),
  M = 10, nrep = 10)
}
\arguments{
  \item{X}{
numeric matrix of predictors.
}
  \item{Y}{
a factor giving the class of each individual.
}
  \item{pred.method}{
prediction method to be applied for PLS-DA cross-validation. Should be a subset of \code{"mahalanobis.dist"} (default), \code{"centroids.dist"} or \code{"max.dist"}.
}
  \item{M}{
the number of folds in the M-fold cross-validation.
}
  \item{nrep}{
the number of repetitions of the whole procedure in the M-fold cross-validation.
}
}
\details{
The function emphasizes values obtained with \code{nlev - 1} groups, with \code{nlev} being the number of levels of the factor, which is often the optimal number of components to be kept.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[mixOmics]{plsda}}, \code{\link[mixOmics]{perf}}, \code{\link[RVAideMemoire]{DA.valid}}, \code{\link[RVAideMemoire]{DA.var}}
}
\examples{
require(mixOmics)
data(yeast)

X <- t(yeast$data)
Y <- yeast$strain.cond
# PLSDA.ncomp(X,Y)
}