\name{DA.valid}
\alias{DA.valid}
\alias{print.DA.valid}
\title{
Cross-validation in Linear Discriminant Analysis
}
\description{
Perform a cross-validation to assess the prediction ability of a Linear Discriminant Analysis (obtained from the function \code{lda()} of the package MASS). Two methods are implemented for cross-validation: leave-one-out and M-fold.
}
\usage{
DA.valid(model, method = c("loo", "Mfold"), crit.lda = c("plug-in", "predictive", "debiased"), M = 10, nrep = 20)
}
\arguments{
  \item{model}{
object of class \code{"lda"} (from the function \code{lda()} of the package MASS).
}
  \item{method}{
what kind of validation to use, matching one of \code{"Mfold"} or \code{"loo"} (see below). Default is \code{"Mfold"}.
}
  \item{crit.lda}{
this determines how the parameter estimation is handled in LDA cross-validation. With \code{"plug-in"} (the default) the usual unbiased parameter estimates are used and assumed to be correct. With \code{"debiased"} an unbiased estimator of the log posterior probabilities is used, and with \code{"predictive"} the parameter estimates are integrated out using a vague prior. 
}
  \item{M}{
the number of folds in the M-fold cross-validation.
}
  \item{nrep}{
the number of repetitions of the whole procedure in the M-fold cross-validation.
}
}
\value{
  \item{model}{type of discriminant model used.}
  \item{method}{method used for cross-validation.}
  \item{crit.lda}{method for parameter estimation in LDA cross-validation.}
  \item{M}{number of folds in the M-fold cross-validation.}
  \item{nrep}{number of repetitions in the M-fold cross-validation.}
  \item{tab}{results of cross-validation.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}}
}
\examples{
# Cross-validation of a LDA model
require(MASS)
data(iris)
model.LDA <- lda(iris[,1:4],iris$Species)
DA.valid(model.LDA)
}
