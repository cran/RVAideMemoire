\name{DA.valid}
\alias{DA.valid}
\alias{print.DA.valid}
\title{
Cross-validation in Discriminant Analysis
}
\description{
Perform a cross-validation to assess the prediction ability of a Discriminant Analysis. Linear Discriminant Analysis (by using \code{\link[MASS]{lda}}) and Partial Least Squares - Discriminant Analysis (by using \code{\link[mixOmics]{plsda}}) are handled. Two methods are implemented for cross-validation: leave-one-out and M-fold.
}
\usage{
DA.valid(model, method = c("loo", "Mfold"), crit.lda = c("plug-in", "predictive",
  "debiased"), crit.plsda = c("mahalanobis.dist", "centroids.dist", "max.dist"),
  M = 10, nrep = 20)
}
\arguments{
  \item{model}{
object of class \code{"lda"} (from \code{\link[MASS]{lda}}) or \code{"plsda"} (from \code{\link[mixOmics]{plsda}}).
}
  \item{method}{
what kind of validation to use, matching one of \code{"Mfold"} or \code{"loo"} (see below). Default is \code{"Mfold"}.
}
  \item{crit.lda}{
this determines how the parameter estimation is handled in LDA cross-validation. With \code{"plug-in"} (the default) the usual unbiased parameter estimates are used and assumed to be correct. With \code{"debiased"} an unbiased estimator of the log posterior probabilities is used, and with \code{"predictive"} the parameter estimates are integrated out using a vague prior. 
}
  \item{crit.plsda}{
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
Leave-one-out cross-validation is not recommended with PLS-DA models.

When working on a LDA model, the prior probabilities for the models fitted on the training datasets are extracted from \code{model} (they are not evaluated from the training dataset itself).
}
\value{
  \item{model}{type of discriminant model used.}
  \item{method}{method used for cross-validation.}
  \item{crit.lda}{method for parameter estimation in LDA cross-validation.}
  \item{crit.plsda}{distance used in PLS-DA cross-validation.}
  \item{M}{number of folds in the M-fold cross-validation.}
  \item{nrep}{number of repetitions in the M-fold cross-validation.}
  \item{tab}{results of cross-validation.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}}, \code{\link[mixOmics]{plsda}}, \code{\link[mixOmics]{valid}}
}
\examples{
# Cross-validation of a LDA model
require(MASS)
data(iris)
model.LDA <- lda(iris[,1:4],iris$Species)
DA.valid(model.LDA,M=5,nrep=10)

# Cross-validation of a PLS-DA model
require(mixOmics)
data(yeast)
model.PLSDA <- plsda(t(yeast$data),yeast$strain.cond)
DA.valid(model.PLSDA,M=5,nrep=10)
}
