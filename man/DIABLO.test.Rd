\name{DIABLO.test}
\alias{DIABLO.test}
\title{
Significance test based on cross-validation
}
\description{
Performs a permutational significance test based on cross-validation with DIABLO (\code{\link[mixOmics]{block.plsda}} or \code{\link[mixOmics]{block.splsda}}).
}
\usage{
DIABLO.test(x, method = c("mahalanobis.dist", "max.dist", "centroids.dist"),
  validation = c("Mfold", "loo"), k = 7, nperm = 999, progress = TRUE, ...)
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
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
  \item{\dots}{
other arguments to pass to \code{\link[mixOmics]{perf}}.
}
}
\details{
The function uses the weighted predicted error rate (see \code{\link[mixOmics]{perf}}).
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name of the data, plus additional information.}
  \item{statistic}{the value of the test statistics.}
  \item{permutations}{the number of permutations.}
  \item{p.value}{the p-value of the test.}
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
DIABLO.test(DIABLO)
}
}
