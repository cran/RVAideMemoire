\name{DA.confusion}
\alias{DA.confusion}
\title{
Classification error rate of a Discriminant Analysis
}
\description{
Computes the classification error rate of a Discriminant Analysis. The function divides the data in a training and a testing dataset, and predict the class of the individuals of the testing dataset by using the model fitted with the training dataset. Linear Discriminant Analysis (from \code{\link[MASS]{lda}}), Partial Least Squares - Discriminant Analysis (from \code{\link[mixOmics]{plsda}}) and Correspondence Discriminant Analysis (from \code{\link[ade4]{discrimin.coa}}) are handled.
}
\usage{
DA.confusion(model, train = 2/3, crit.lda = c("plug-in", "predictive","debiased"),
  crit.plsda = c("mahalanobis.dist", "centroids.dist", "max.dist"),
  crit.cda = c("mahalanobis", "euclidian"))
}
\arguments{
  \item{model}{
object of class \code{"lda"} (from \code{\link[MASS]{lda}}), \code{"plsda"} (from \code{\link[mixOmics]{plsda}}) or \code{"coadisc"} (from \code{\link[ade4]{discrimin.coa}}).
}
  \item{train}{
proportion of the total number of individuals to be used to build the training dataset.
}
  \item{crit.lda}{
this determines how the parameter estimation is handled in LDA cross-validation. With \code{"plug-in"} (the default) the usual unbiased parameter estimates are used and assumed to be correct. With \code{"debiased"} an unbiased estimator of the log posterior probabilities is used, and with \code{"predictive"} the parameter estimates are integrated out using a vague prior. 
}
  \item{crit.plsda}{
prediction method to be applied for PLS-DA cross-validation. Should be a subset of \code{"mahalanobis.dist"} (default), \code{"centroids.dist"} or \code{"max.dist"}.
}
  \item{crit.cda}{
prediction method to be applied for CDA cross-validation. Should be a subset of \code{"mahalanobis"} (default) or \code{"euclidian"}.
}
}
\details{
When working on a LDA, the prior probabilities for the model fitted on the training dataset are extracted from \code{model} (they are not evaluated from the training dataset itself).
}
\value{
  \item{model}{type of discriminant model used.}
  \item{crit.lda}{method for parameter estimation when working on a LDA.}
  \item{crit.plsda}{distance used when working on a PLS-DA.}
  \item{crit.cda}{distance used in CDA cross-validation.}
  \item{prop.train}{a vector giving the number of individuals used to build the training dataset (\code{used}) and the total number of individuals (\code{total}).}
  \item{ind.for.train}{individuals used to build the training dataset.}
  \item{predicted}{vector of predicted classes.}
  \item{confusion}{confusion matrix.}
  \item{prop.confusion}{classification error rate.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[MASS]{lda}}, \code{\link[MASS]{predict.lda}}, \code{\link[mixOmics]{plsda}}, \code{\link[mixOmics]{predict.plsda}}, \code{\link[ade4]{discrimin.coa}}, \code{\link[RVAideMemoire]{predict.coadisc}}
}
\examples{
# With a LDA
require(MASS)
data(iris)

model.LDA <- lda(iris[,1:4],iris$Species)
DA.confusion(model.LDA)
}
