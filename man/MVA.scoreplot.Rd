\name{MVA.scoreplot}
\alias{MVA.scoreplot}
\title{
Score plot of multivariate analyses
}
\description{
Displays a score plot of a multivariate analysis.
}
\usage{
MVA.scoreplot(x, xax = 1, yax = 2, scaling = 2, set = c(12, 1, 2), space = 1,
  byfac = TRUE, fac = NULL, barycenters = TRUE, stars = TRUE, contours = FALSE,
  dhist = TRUE, weights = 1, xlab = NULL, ylab = NULL, main = NULL, pch = 16,
  cex = 1, col = 1, points = TRUE, labels = NULL, main.pos = c("bottomleft",
  "topleft", "bottomright", "topright"), main.cex = 1.3, fac.lab = NULL,
  fac.cex = 1, legend = FALSE, legend.pos = c("topleft", "topright", "bottomleft",
  "bottomright"), legend.title = NULL, legend.lab = NULL, legend.cex = 1,
  drawextaxes = TRUE, drawintaxes = TRUE, xlim = NULL, ylim = NULL,
  keepmar = FALSE)
}
\arguments{
  \item{x}{
  a multivariate analysis (see Details).
}
  \item{xax}{
  the horizontal axis.
}
  \item{yax}{
  the vertical axis. This can be set to \code{NULL} for a one-dimensional graph. The type of graph to be drawn in this case depends on the value of \code{dhist}.
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
  \item{byfac}{
  only used with MCA and mix analyses (see Details). If \code{TRUE}, a separate score plot is displayed for each factor included in the analysis. In this case \code{fac} cannot be used and if \code{main=NULL}, the factor names are displayed as titles on the graphs.
}
  \item{fac}{
  an optional factor defining groups of individuals.
}
  \item{barycenters}{
  only used if \code{fac} is not \code{NULL}. If \code{TRUE} (default), the name of each group (defined by \code{fac.lab}) is diplayed at the position of the barycenter of this group. Available for two-dimensional graphs and for dotcharts in the one-dimensional case (see \code{dhist}).
}
  \item{stars}{
  only used if \code{fac} is not \code{NULL}. If \code{TRUE} (default), the individual of each group are linked to their corresponding barycenter.
}
  \item{contours}{
  only used if \code{fac} is not \code{NULL}. If \code{TRUE}, a polygon of contour is displayed for each group.
}
  \item{dhist}{
  only used in the one-dimensional case. If \code{TRUE} (default), a density histogram is displayed. If \code{FALSE}, a dotchart is displayed.
}
  \item{weights}{
  individual weights, used to calculate barycenter positions (see \code{barycenters}).
}
  \item{xlab}{
  legend of the horizontal axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis.
}
  \item{ylab}{
  legend of the vertical axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis. Available for two-dimensional graphs and for density histograms in the one-dimensional case (see \code{dhist}).
}
  \item{main}{
  optional title of the graph. Can be a vector of several values for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{pch}{
  symbol(s) used for points, when points are displayed (see \code{points}). If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary. Available for two-dimensional graphs and for dotcharts in the one-dimensional case (see \code{dhist}). Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{cex}{
  size of the points or of the labels (see \code{points}). Available for two-dimensional graphs and for dotcharts in the one-dimensional case (see \code{dhist}). For two-dimensional graphs: if \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group; otherwise a vector of any length can be defined, which is recycled if necessary. For dotcharts, gives the size used for points and all labels (see \code{\link{dotchart}}). Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{col}{
  color(s) used for points or labels (see \code{points}). If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary (not available for density histograms, see \code{dhist}). Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{points}{
  only used for two-dimensional graphs. If \code{FALSE}, points are replaced with their corresponding label (defined by \code{labels}). Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{labels}{
  used in two-dimensional graphs when \code{points=FALSE} and in dotcharts (see \code{dhist}). Names of the individuals. If \code{NULL} (default), labels correspond to row names of the data used in the multivariate analysis. Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{main.pos}{
  position of the title, if \code{main} is not \code{NULL}. Default to \code{"bottomleft"}. Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{main.cex}{
  size of the title, if \code{main} is not \code{NULL}. Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{fac.lab}{
  only used if \code{fac} is not \code{NULL}. Labels used to display barycenters in two-dimensional graphs or on the vertical axis of a dotchart in the one-dimensional case (see \code{dhist}). If \code{NULL}, levels of the factor defined by \code{fac} are used. In case of a MCA or a mix analysis with \code{byfac=TRUE} (see \code{byfac}), labels cannot be changed and correspond to the levels of the factor displayed on each graph.
}
  \item{fac.cex}{
  only used if \code{fac} is not \code{NULL} and in two-dimensional graphs. Labels used to display barycenters. Can be a vector of length one or a vector giving one value per group. Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{legend}{
  logical indicating if a legend should be added to the graph. Available for two-dimensional graphs and for density histograms in the one-dimensional case (see \code{dhist}).
}
  \item{legend.pos}{
  position of the legend, if \code{legend} is \code{TRUE}. Default to \code{"topleft"}.
}
  \item{legend.title}{
  optional title of the legend, if \code{legend} is \code{TRUE}. Not available for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{legend.lab}{
  legend labels, if \code{legend} is \code{TRUE}. If \code{NULL}, labels defined by \code{fac.labels} are used (see \code{fac.labels}).
}
  \item{legend.cex}{
  size of legend labels, if \code{legend} is \code{TRUE}.
}
  \item{drawextaxes}{
  logical indicating if external axes should be drawn. Available for two-dimensional graphs and for density histograms in the one-dimensional case (see \code{dhist}).
}
  \item{drawintaxes}{
  logical indicating if internal axes should be drawn.
}
  \item{xlim}{
  limits of the horizontal axis. If \code{NULL}, limits are computed automatically. Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{ylim}{
  only used in two-dimensional graphs. Limits of the vertical axis. If \code{NULL}, limits are computed automatically. Re-used for all graphs for MCA and mix analyses when \code{byfac=TRUE} (see \code{byfac}).
}
  \item{keepmar}{
  only used in two-dimensional graphs. Logical indicating if margins defined by MVA.scoreplot should be kept after plotting (necessary for biplots).
}
}
\details{
This function should not be use directly. Prefer the general \code{\link[RVAideMemoire]{MVA.plot}}, to which all arguments can be passed.

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
\examples{
data(iris)
PCA <- prcomp(iris[,1:4])
MVA.plot(PCA,"scores")
MVA.plot(PCA,"scores",fac=iris$Species,col=1:3,pch=15:17)
}