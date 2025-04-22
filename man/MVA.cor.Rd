\name{MVA.cor}
\alias{MVA.cor}
\title{
Correlations of multivariate analyses
}
\description{
Returns correlations of a multivariate analysis.
}
\usage{
MVA.cor(x, xax = 1, yax = 2, set = c(12, 1, 2), space = 1, ...)
}
\arguments{
  \item{x}{
  a multivariate analysis (see Details).
}
  \item{xax}{
  axis or axes for which to extract correlations.
}
  \item{yax}{
  axis for which to extract correlations (ignored if \code{length(xax) > 1}).
}
  \item{set}{
  variables to be displayed, when several sets are available (see Details). \code{12} (default) for both sets, \code{1} for X or constraints, \code{2} for Y or constrained variables.
}
  \item{space}{
  variables to be displayed, when several spaces are available (see Details). \code{space} is the number of the space to be plotted.
}
  \item{\dots}{
  not used.
}
}
\details{
Many multivariate analyses are supported, from various packages:

- PCA: \code{\link[ade4]{dudi.pca}}, \code{\link[vegan:cca]{rda}}.

- sPCA: \code{\link[mixOmics]{spca}}.

- IPCA: \code{\link[mixOmics]{ipca}}.

- sIPCA: \code{\link[mixOmics]{sipca}}.

- LDA: \code{\link[MASS]{lda}}, \code{\link[ade4]{discrimin}}.

- PLS-DA (PLS2 on a dummy-coded factor): \code{\link[mixOmics]{plsda}}. X space only.

- sPLS-DA (sPLS2 on a dummy-coded factor): \code{\link[mixOmics]{splsda}}. X space only.

- CPPLS: \code{\link[pls]{mvr}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. X space only.

- PLSR: \code{\link[pls]{mvr}}, \code{\link[mixOmics]{pls}}, \code{plsR} (plsRglm package). Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. X space only.

- sPLSR: \code{\link[mixOmics]{pls}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. X space only.

- PLS-GLR: \code{plsRglm} (plsRglm package). Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. Correlations are computed with Y on the link scale.

- PCR: \code{\link[pls]{mvr}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- CDA: \code{\link[ade4]{discrimin}}, \code{\link[ade4]{discrimin.coa}}.

- NSCOA: \code{\link[ade4]{dudi.nsc}}. For NSCOA there is no real correlation, but the classical representation of columns is arrows. This is why MVA.corplot was made able to deal with this analysis.

- CCA: \code{\link[vegan]{cca}}, \code{\link[ade4]{pcaiv}}. Constraints (only quantitative constraints are extracted) in constrained space only.

- Mix analysis: \code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{dudi.hillsmith}}. Only quantitative variables are displayed.

- RDA (or PCAIV): \code{\link[ade4]{pcaiv}}, \code{\link[ade4]{pcaivortho}}, \code{\link[vegan:cca]{rda}}. With \code{\link[vegan:cca]{rda}}, space 1 is constrained space, space 2 is unconstrained space. Only constrained space is available with \code{\link[ade4]{pcaiv}}, the opposite for \code{\link[ade4]{pcaivortho}}. Set 1 is constraints (only quantitative constraints are extracted), set 2 is dependent variables (only set 2 is available for \code{\link[ade4]{pcaivortho}}). If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- CCorA: \code{\link[vegan]{CCorA}}, \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y. With \code{\link[mixOmics]{rcc}} a third space is available, in which coordinates are means of X and Y coordinates. In this third space, set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- rCCorA: \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates. In space 3, set 1 is X and set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- CIA: \code{\link[ade4]{coinertia}}. Space 1 is X, space 2 is Y, space 3 is a "common" space where X and Y scores are normed. In space 3, set 1 is X and set 2 is Y. If \code{set=12} in space 3 (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lws} can be defined differently for each set.

- GPA: \code{\link[FactoMineR]{GPA}}. Only the consensus ordination can be displayed.

- 2B-PLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates. In space 3, set 1 is X and set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- 2B-sPLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates. In space 3, set 1 is X and set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- rGCCA: \code{\link[mixOmics]{wrapper.rgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).

- sGCCA: \code{\link[mixOmics]{wrapper.sgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).

- DIABLO: \code{\link[mixOmics]{block.plsda}}, \code{\link[mixOmics]{block.splsda}}. Space can be 1 to n, the number of blocks (i.e. datasets).
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}