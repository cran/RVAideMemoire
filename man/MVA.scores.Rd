\name{MVA.scores}
\alias{MVA.scores}
\title{
Scores of multivariate analyses
}
\description{
Returns scores of a multivariate analysis.
}
\usage{
MVA.scores(x, xax = 1, yax = 2, scaling = 2, set = c(12, 1, 2), space = 1, ...)
}
\arguments{
  \item{x}{
  a multivariate analysis (see Details).
}
  \item{xax}{
  axis or axes for which to extract scores.
}
  \item{yax}{
  axis for which to extract scores (ignored if \code{length(xax) > 1}).
}
  \item{scaling}{
  type of scaling. Only available with some analyses performed with the \code{vegan} package. See Details.
}
  \item{set}{
  scores to be displayed, when several sets are available (see Details). \code{12} (default) for both sets, \code{1} for rows or X, \code{2} for columns or Y.
}
  \item{space}{
  scores to be displayed, when several spaces are available (see Details). \code{space} is the number of the space to be plotted.
}
  \item{\dots}{
  not used.
}
}
\details{
Many multivariate analyses are supported, from various packages:

- PCA: \code{\link{prcomp}}, \code{\link{princomp}} (if \code{scores=TRUE}), \code{\link[ade4]{dudi.pca}}, \code{\link[vegan]{rda}}, \code{\link[mixOmics]{pca}}, \code{\link[labdsv]{pca}}. \code{scaling} can be defined for \code{\link[vegan]{rda}} (see \code{\link[vegan]{scores.rda}}).

- sPCA: \code{\link[mixOmics]{spca}}.

- IPCA: \code{\link[mixOmics]{ipca}}.

- sIPCA: \code{\link[mixOmics]{sipca}}.

- PCoA: \code{\link{cmdscale}} (with at least on non-default argument), \code{\link[ade4]{dudi.pco}}, \code{\link[vegan]{wcmdscale}} (with at least one non-default argument), \code{\link[vegan]{capscale}}, \code{\link[labdsv]{pco}}, \code{\link[ape]{pcoa}}.

- nMDS: \code{\link[vegan]{monoMDS}}, \code{\link[vegan]{metaMDS}}, \code{\link[labdsv]{nmds}}, \code{\link[MASS]{isoMDS}}.

- LDA: \code{\link[MASS]{lda}}, \code{\link[ade4]{discrimin}}.

- PLS-DA (PLS2 on a dummy-coded factor): \code{\link[mixOmics]{plsda}}. X space only.

- sPLS-DA (sPLS2 on a dummy-coded factor): \code{\link[mixOmics]{splsda}}. X space only.

- CPPLS: \code{\link[pls]{mvr}}. X space only.

- PLSR: \code{\link[pls]{mvr}}, \code{\link[mixOmics]{pls}}, \code{plsR} (plsRglm package). X space only.

- sPLSR: \code{\link[mixOmics]{pls}}. X space only.

- PLS-GLR: \code{plsRglm} (plsRglm package).

- PCR: \code{\link[pls]{mvr}}.

- CDA: \code{\link[ade4]{discrimin}}, \code{\link[ade4]{discrimin.coa}}.

- NSCOA: \code{\link[ade4]{dudi.nsc}}.

- MCA: \code{\link[ade4]{dudi.acm}}.

- Mix analysis: \code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{dudi.hillsmith}}.

- COA: \code{\link[ade4]{dudi.coa}}, \code{\link[vegan]{cca}}. Set 1 is rows, set 2 is columns. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col} can be defined differently for each set. \code{scaling} can be defined for \code{\link[vegan]{cca}} (see \code{\link[vegan]{scores.cca}}).

- DCOA: \code{\link[ade4]{dudi.dec}}. Set 1 is rows, set 2 is columns. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col} can be defined differently for each set.

- PCIA: \code{\link[ade4]{procuste}}. Set 1 is X, set 2 is Y.  If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col} can be defined differently for each set.

- Procrustean superimposition: \code{\link[vegan]{procrustes}}. Set 1 is X, set 2 is Y.  If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col} can be defined differently for each set.

- GPA: \code{\link[FactoMineR]{GPA}}. Only the consensus ordination can be displayed.

- DPCoA: \code{\link[ade4]{dpcoa}}. Set 1 is categories, set 2 is collections.  If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col} can be defined differently for each set.

- RDA (or PCAIV): \code{\link[ade4]{pcaiv}}, \code{\link[ade4]{pcaivortho}}, \code{\link[vegan]{rda}}. With \code{\link[vegan]{rda}}, space 1 is constrained space, space 2 is unconstrained space. Only constrained space is available with \code{\link[ade4]{pcaiv}}, the opposite for \code{\link[ade4]{pcaivortho}}. \code{scaling} can be defined for \code{\link[vegan]{rda}} (see \code{\link[vegan]{scores.rda}}).

- db-RDA (or CAP): \code{\link[vegan]{capscale}}, \code{\link[vegan]{dbrda}}. Space 1 is constrained space, space 2 is unconstrained space.

- CCA: \code{\link[ade4]{pcaiv}}, \code{\link[vegan]{cca}}. With \code{\link[vegan]{rda}}, space 1 is constrained space, space 2 is unconstrained space. Only constrained space is available with \code{\link[ade4]{pcaiv}}. Set 1 is rows, set 2 is columns. \code{scaling} can be defined for \code{\link[vegan]{cca}} (see \code{\link[vegan]{scores.cca}}).

- CCorA: \code{\link[vegan]{CCorA}}, \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y. With \code{\link[mixOmics]{rcc}} a third space is available, in which coordinates are means of X and Y coordinates.

- rCCorA: \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates.

- CIA: \code{\link[ade4]{coinertia}}. Space 1 is X, space 2 is Y, space 3 is a "common" space where X and Y scores are normed. In space 3, set 1 is X and set 2 is Y. If \code{set=12} in space 3 (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col} can be defined differently for each set.

- 2B-PLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates.

- 2B-sPLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates.

- rGCCA: \code{\link[RGCCA]{rgcca}}, \code{\link[mixOmics]{wrapper.rgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).

- sGCCA: \code{\link[RGCCA]{sgcca}}, \code{\link[mixOmics]{wrapper.sgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).

- DIABLO: \code{\link[mixOmics]{block.plsda}}, \code{\link[mixOmics]{block.splsda}}. Space can be 1 to n, the number of blocks (i.e. datasets).
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}