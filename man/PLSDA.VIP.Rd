\name{PLSDA.VIP}
\alias{PLSDA.VIP}
\title{
Variable Importance in the Projection (VIP)
}
\description{
Return VIP score of each X-variable in a PLS-DA model (obtained from \code{\link[mixOmics]{plsda}}).
}
\usage{
PLSDA.VIP(model, graph = FALSE)
}
\arguments{
  \item{model}{
object of class \code{"plsda"} (from \code{\link[mixOmics]{plsda}}).
}
  \item{graph}{
logical: should VIP scores be printed?
}
}
\value{
  \item{tab}{table of results.}
  \item{sup1}{name of X-variables having a VIP score > 1.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[mixOmics]{plsda}}
}
\examples{
require(mixOmics)
data(yeast)
model.PLSDA <- plsda(t(yeast$data),yeast$strain.cond)
PLSDA.VIP(model.PLSDA)
}
