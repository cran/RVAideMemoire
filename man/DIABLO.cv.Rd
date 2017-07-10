\name{DIABLO.cv}
\alias{DIABLO.cv}
\title{
Cross validation
}
\description{
Performs cross validation with DIABLO (\code{\link[mixOmics]{block.plsda}} or \code{\link[mixOmics]{block.splsda}}).
}
\usage{
DIABLO.cv(x, method = c("mahalanobis.dist", "max.dist", "centroids.dist"),
  validation = c("Mfold", "loo"), k = 7, repet = 10, ...)
}
\arguments{
  \item{x}{
an object of class \code{"sgccda"}.
}
  \item{method}{
criterion used to predict class membership. See \code{\link[mixOmics]{perf}}.
}
  \item{validation}{
a character giving the kind of (internal) validation to use. See \code{\link[mixOmics]{perf}}.
}
  \item{k}{
an integer giving the number of folds (can be re-set internally if needed).
}
  \item{repet}{
an integer giving the number of times the whole procedure has to be repeated.
}
  \item{\dots}{
other arguments to pass to \code{\link[mixOmics]{perf}}.
}
}
\details{
The function uses the weighted predicted classification error rate (see \code{\link[mixOmics]{perf}}).
}
\value{
  \item{repet}{number of times the whole procedure was repeated.}
  \item{k}{number of folds.}
  \item{validation}{kind of validation used.}
  \item{ncomp}{number of components used.}
  \item{method}{criterion used to classify individuals of the test sets.}
  \item{NMC.mean}{mean classification error rate (based on \code{repet} values).}
  \item{NMC.se}{standard error of the classification error rate (based on \code{repet} values).}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[mixOmics]{block.plsda}}, \code{\link[mixOmics]{block.splsda}}, \code{\link[mixOmics]{perf}}
}
\examples{
\dontrun{
require(mixOmics)
data(nutrimouse)
data <- list(gene=nutrimouse$gene,lipid=nutrimouse$lipid,Y=nutrimouse$diet)
DIABLO <- block.plsda(X=data,indY=3)
DIABLO.cv(DIABLO)
}
}