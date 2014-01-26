\name{PLSDA.test}
\alias{PLSDA.test}
\title{
Permutational test for the discriminant ability of the factor in PLS-DA
}
\description{
Perform a permutational test based on the classification error rate of a PLS-DA model, to test for the discriminant ability of the factor.
}
\usage{
PLSDA.test(model, pred.method = c("mahalanobis.dist", "centroids.dist", "max.dist"),
  M = 10, nperm = 999)
}
\arguments{
  \item{model}{
object of class \code{"plsda"} (from \code{\link[mixOmics]{plsda}}).
}
  \item{pred.method}{
prediction method to be applied for PLS-DA cross-validation. Should be a subset of \code{"mahalanobis.dist"} (default), \code{"centroids.dist"} or \code{"max.dist"}.
}
  \item{M}{
the number of folds in the M-fold cross-validation.
}
  \item{nperm}{
number of permutations.
}
}
\details{
Data have to be balanced to perform the test. The function automatically takes care of it by taking equal number of individuals in each class of the factor, randomly chosen for each class and in each permutation. The number of individuals kept is equal to the number of individuals in the smallest class.

To compare classification error rates of the original and permuted models, classification error rate of the original model must be obtained with balanced data. The function does this by randomly choosing equal number of individuals in each class (as in the permutation process). However, to avoid basing the test on an original classification error rate obtained from non representative individuals, the process is repeated 30 times and the mean of these 30 values is considered as the classification error rate of the original model.
}
\value{
  \item{method}{a character string giving the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data and parameters of the cross-validation.}
  \item{p.value}{p-value of the permutational test.}
  \item{alternative}{a character string describing the alternative hypothesis, always \code{"greater"}.}
  \item{null.value}{the classification error rate obtained from the original model.}
  \item{estimate}{the classification error rate obtained from the original model.}
  \item{pred.method}{distance criterion used in cross-validation.}
  \item{M}{number of folds in the M-fold cross-validation.}
  \item{permutations}{number of permutations.}
}
\section{Warning}{
The process is quite long and takes several minutes to be achieved. Just don't worry.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[mixOmics]{plsda}}, \code{\link[mixOmics]{valid}}
}
\examples{
# Not enough permutations here but it runs faster.

require(mixOmics)
data(yeast)
X <- t(yeast$data)
Y <- yeast$strain.cond
model <- plsda(X,Y)
\dontrun{PLSDA.test(model,M=3,nperm=29)}
}