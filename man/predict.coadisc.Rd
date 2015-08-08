\name{predict.coadisc}
\alias{predict.coadisc}
\title{
Predict method for CDA
}
\description{
Predicts class of the grouping factor based on a Correspondence Discriminant Analysis (performed using \code{\link[ade4]{discrimin.coa}}).
}
\usage{

\method{predict}{coadisc}(object, newdata, dim=object$nf, method = c("mahalanobis", "euclidian"), ...)

}
\arguments{
  \item{object}{
object of class inheriting from \code{"coadisc"}.
}
  \item{newdata}{
contingency table (either a \code{"matrix"}, \code{"table"} or \code{"data.frame"} object) giving new individuals (one row per individual).
}
  \item{dim}{
number of dimensions to be used for prediction.
}
  \item{method}{
distance metric to be used for prediction. In all cases the predicted class corresponds to the minimum distance between the new individual and the centroid of each class. Default is Mahalanobis distance.
}
  \item{\dots}{
further arguments to be passed to or from other methods.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[ade4]{discrimin.coa}}
}
\examples{
require(ade4)
data(perthi02)

CDA <- discrimin.coa(perthi02$tab,perthi02$cla,scan=FALSE)
new <- matrix(c(17,45,32,17,17,52,28,29,6,10,7,7,7,5,10,4,37,34,23,9),ncol=20)
predict(CDA,new)
}