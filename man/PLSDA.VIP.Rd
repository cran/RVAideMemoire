\name{PLSDA.VIP}
\alias{PLSDA.VIP}
\alias{print.PLSDA.VIP}
\title{
Variable Importance in the Projection (VIP)
}
\description{
Return VIP score of each X-variable in a PLS-DA model (obtained from the function \code{plsda()} of the package mixOmics).
}
\usage{
PLSDA.VIP(model, graph = FALSE)
}
\arguments{
  \item{model}{
object of class \code{"plsda"} (from the function \code{plsda()} of the package mixOmics).
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
Maxime Herv� <mx.herve@gmail.com>
}
\seealso{
\code{\link[mixOmics]{plsda}}
}
\examples{
require(mixOmics)
data(yeast)
model.PLSDA <- plsda(t(yeast$data),yeast$strain.cond,ncomp=3)
PLSDA.VIP(model.PLSDA)
}