\name{CDA.cv}
\alias{CDA.cv}
\title{
Cross validation
}
\description{
Performs cross validation with correspondence discriminant analyses.
}
\usage{
CDA.cv(X, Y, repet = 10, k = 7, ncomp = NULL, method = c("mahalanobis",
  "euclidian"))
}
\arguments{
  \item{X}{
a data frame of dependent variables (typically contingency or presence-absence table).
}
  \item{Y}{
factor giving the groups.
}
  \item{repet}{
an integer giving the number of times the whole procedure has to be repeated.
}
  \item{k}{
an integer giving the number of folds (can be re-set internally if needed).
}
  \item{ncomp}{
an integer giving the number of components to be used for prediction. If \code{NULL} all components are used.
}
  \item{method}{
criterion used to predict class membership. See \code{\link[RVAideMemoire]{predict.coadisc}}.
}
}
\details{
The training sets are generated in respect to the relative proportions of the levels of \code{Y} in the original data set (see \code{\link[RVAideMemoire]{splitf}}).
}
\value{
  \item{repet}{number of times the whole procedure was repeated.}
  \item{k}{number of folds.}
  \item{ncomp}{number of components used.}
  \item{method}{criterion used to classify individuals of the test sets.}
  \item{groups}{levels of \code{Y}.}
  \item{models.list}{list of of models generated (\code{repet*k} models), for PLSR, CPPLS, PLS-DA, PPLS-DA, LDA and QDA.}
  \item{NMC}{NMC values (\code{repet} values).}
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
\dontrun{CDA.cv(perthi02$tab,perthi02$cla)}
}