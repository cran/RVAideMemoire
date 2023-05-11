\name{ord.rw}
\alias{ord.rw}
\title{
Re-computation of an ordination using given row weights
}
\description{
Re-computes an ordination using given row weights (possibly extracted from a correspondence analysis). The function is intended to be used prior to \code{\link[ade4]{coinertia}} when row weights have to be equalized.
}
\usage{
ord.rw(ord, CA = NULL, rw = NULL)
}
\arguments{
  \item{ord}{
  an ordination to re-compute. Must come from the \code{ade4} package or be supported by \code{\link[RVAideMemoire]{to.dudi}}. In any case the resulting ordination will be in the \code{ade4} format.
}
  \item{CA}{
  an optional correspondence analysis from which row weights should be extracted. Must come from \code{\link[ade4]{dudi.coa}} or \code{\link[vegan]{cca}}.
}
  \item{rw}{
  an optional vector of row weights. Used only is \code{CA} is \code{NULL}.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}