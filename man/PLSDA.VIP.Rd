\name{PLSDA.VIP}
\alias{PLSDA.VIP}
\title{
Variable Importance in the Projection (VIP)
}
\description{
Returns VIP score of each X-variable in a PLS-DA (obtained from \code{\link[mixOmics]{plsda}}).
}
\usage{
PLSDA.VIP(model, graph = FALSE)
}
\arguments{
  \item{model}{
object of class \code{"plsda"} (from \code{\link[mixOmics]{plsda}}).
}
  \item{graph}{
logical: should VIP scores be displayed?
}
}
\value{
  \item{tab}{table of results.}
  \item{sup1}{name of X-variables having a VIP score > 1.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[mixOmics]{plsda}}
}
\examples{
if (require(mixOmics)) {
  data(yeast)
  model.PLSDA <- plsda(t(yeast$data),yeast$strain.cond)
  PLSDA.VIP(model.PLSDA)
}
}
