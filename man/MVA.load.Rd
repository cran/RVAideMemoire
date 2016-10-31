\name{MVA.load}
\alias{MVA.load}
\title{
Loadings of multivariate analyses
}
\description{
Returns loadings of a multivariate analysis.
}
\usage{
MVA.load(x, xax = 1, yax = 2, set = c(12, 1, 2), space = 1, ...)
}
\arguments{
  \item{x}{
  a multivariate analysis (see Details).
}
  \item{xax}{
  the horizontal axis.
}
  \item{yax}{
  the vertical axis.
}
  \item{set}{
  variables to be displayed, when several sets are available (see Details). \code{12} (default) for both sets, \code{1} for X, \code{2} for Y.
}
  \item{space}{
  variables to be displayed, when several spaces are available (see Details). \code{space} is the number of the space to be plotted.
}
  \item{...}{
  not used.
}
}
\details{
Many multivariate analyses are supported, from various packages:

- PCA: \code{\link{prcomp}}, \code{\link{princomp}}, \code{\link[ade4]{dudi.pca}}, \code{\link[vegan]{rda}}, \code{\link[mixOmics]{pca}}, \code{\link[labdsv]{pca}}.

- sPCA: \code{\link[mixOmics]{spca}}.

- IPCA: \code{\link[mixOmics]{ipca}}.

- sIPCA: \code{\link[mixOmics]{sipca}}.

- LDA: \code{\link[MASS]{lda}}, \code{\link[ade4]{discrimin}}.

- PLS-DA (PLS2 on a dummy-coded factor): \code{\link[mixOmics]{plsda}}. X space only.

- sPLS-DA (sPLS2 on a dummy-coded factor): \code{\link[mixOmics]{splsda}}. X space only.

- CPPLS: \code{\link[pls]{mvr}}. X space only.

- PLSR: \code{\link[pls]{mvr}}, \code{\link[mixOmics]{pls}}, \code{\link[plsRglm]{plsR}}. X space only.

- sPLSR: \code{\link[mixOmics]{pls}}. X space only.

- PLS-GLR: \code{\link[plsRglm]{plsRglm}}.

- PCR: \code{\link[pls]{mvr}}.

- CDA: \code{\link[ade4]{discrimin}}, \code{\link[ade4]{discrimin.coa}}.

- NSCOA: \code{\link[ade4]{dudi.nsc}}.

- MCA: \code{\link[ade4]{dudi.acm}}.

- Mix analysis: \code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{dudi.hillsmith}}.

- PCIA: \code{\link[ade4]{procuste}}. Set 1 is X, set 2 is Y.

- RDA (or PCAIV): \code{\link[ade4]{pcaiv}}, \code{\link[ade4]{pcaivortho}}, \code{\link[vegan]{rda}}. With \code{\link[vegan]{rda}}, space 1 is constrained space, space 2 is unconstrained space. Only constrained space is available with \code{\link[ade4]{pcaiv}}, the opposite for \code{\link[ade4]{pcaivortho}}.

- CCorA: \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y.

- rCCorA: \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y.

- CIA: \code{\link[ade4]{coinertia}}. Space 1 is X, space 2 is Y.

- 2B-PLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y.

- 2B-sPLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y.

- rGCCA: \code{\link[mixOmics]{wrapper.rgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).

- sGCCA: \code{\link[mixOmics]{wrapper.sgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}