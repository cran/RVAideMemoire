\name{to.dudi}
\alias{to.dudi}
\title{
Synthesis quality of multivariate analyses
}
\description{
Converts some ordinations performed with the \code{vegan} package to objects compatible with \code{\link[ade4]{coinertia}}.
}
\usage{
to.dudi(ord)
}
\arguments{
  \item{ord}{
  an ordination (see Details).
}
}
\details{
The function supports:

- PCA computed from \code{\link[vegan]{rda}}. If data were scaled (prior to the analysis or using \code{scale} of \code{\link[vegan]{rda}}) it is assumed that is was with the standard deviation using \code{n-1}; As in \code{\link[ade4]{dudi.pca}}, \code{to.dudi} rescales the data with the standard deviation using \code{n}.

- PCoA computed from \code{\link[vegan]{wcmdscale}}, \code{\link[vegan]{capscale}} or \code{\link[vegan]{dbrda}}.

- CA computed from \code{\link[vegan]{cca}}.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}