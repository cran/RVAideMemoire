\name{predict.CDA.cv}
\alias{predict.CDA.cv}
\title{
Predict method for cross-validated CDA submodels
}
\description{
Predicts response based on CDA (correspondence discriminant analysis) submodels generated by cross validation. The predicted class is given with its probability (computed from the values predicted by all submodels).
}
\usage{

\method{predict}{CDA.cv}(object, newdata, type = c("max", "all"), method = c("mahalanobis",
  "euclidian"), ...)

}
\arguments{
  \item{object}{
object of class inheriting from \code{"CDA.cv"}.
}
  \item{newdata}{
vector, matrix or data frame giving new individuals (one row per individual).
}
  \item{type}{
should the probability of the most probable class be given (\code{"max"}, the default) or the probability of each class (\code{"all"})?
}
  \item{method}{
criterion used to predict class membership. See \code{\link[RVAideMemoire]{predict.coadisc}}.
}
  \item{\dots}{
further arguments to be passed to or from other methods.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{CDA.cv}}, \code{\link[RVAideMemoire]{predict.coadisc}}
}
