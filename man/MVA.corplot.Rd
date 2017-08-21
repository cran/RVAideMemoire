\name{MVA.corplot}
\alias{MVA.corplot}
\title{
Correlation circle of multivariate analyses
}
\description{
Displays a correlation circle of a multivariate analysis.
}
\usage{
MVA.corplot(x, xax = 1, yax = 2, thresh = 0, fac = NULL, set = c(12, 1, 2), space = 1,
  xlab = NULL, ylab = NULL, main = NULL, circle = TRUE, intcircle = 0.5, points = TRUE,
  ident = TRUE, arrows = TRUE, labels = NULL, main.pos = c("bottomleft", "topleft",
  "bottomright", "topright"), main.cex = 1.3, legend = FALSE, legend.pos = c("topleft",
  "topright", "bottomleft", "bottomright"), legend.title = NULL, legend.lab = NULL,
  pch = 16, cex = 1, col = 1, lwd = 1, drawintaxes = TRUE, add = FALSE, add.const = 1,
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
  the vertical axis. This can be set to \code{NULL} for a one-dimensional graph, which is a dotchart.
}
  \item{thresh}{
  threshold (in absolute value of the correlation coefficient) of variables to be plotted.
}
  \item{fac}{
  an optional factor defining groups of variables.
}
  \item{set}{
  variables to be displayed, when several sets are available (see Details). \code{12} (default) for both sets, \code{1} for X or constraints, \code{2} for Y or constrained variables.
}
  \item{space}{
  variables to be displayed, when several spaces are available (see Details). \code{space} is the number of the space to be plotted.
}
  \item{xlab}{
  legend of the horizontal axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis.
}
  \item{ylab}{
  only used for two-dimensional graphs. Legend of the vertical axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis.
}
  \item{main}{
  optional title of the graph.
}
  \item{circle}{
  only used for two-dimensional graphs. Logical indicating if the circle of radius 1 should be plotted.
}
  \item{intcircle}{
  only used for two-dimensional graphs. Vector of one or several values indicating radii of circles to be plotted inside the main circle. Can be set to \code{NULL}.
}
  \item{points}{
  only used for two-dimensional graphs. If \code{FALSE}, arrows or points (see \code{arrows}) are replaced with their corresponding label (defined by \code{labels}).
}
  \item{ident}{
  only used for two-dimensional graphs when \code{points=TRUE}. A logical indicating if variable names should be displayed.
}
  \item{arrows}{
  only used if \code{points=TRUE}. Logical indicating if arrows should be plotted. If \code{FALSE}, points are displayed at the extremity of the arrows.
}
  \item{labels}{
  names of the variables. If \code{NULL} (default), labels correspond to variable names found in the data used in the multivariate analysis. For two-dimensional graphs, only used if \code{ident=TRUE}.
}
  \item{main.pos}{
  position of the title, if \code{main} is not \code{NULL}. Default to \code{"bottomleft"}.
}
  \item{main.cex}{
  size of the title, if \code{main} is not \code{NULL}.
}
  \item{legend}{
  only used for two-dimensional graphs. Logical indicating if a legend should be added to the graph.
}
  \item{legend.pos}{
  position of the legend, if \code{legend} is \code{TRUE}. Default to \code{"topleft"}.
}
  \item{legend.title}{
  optional title of the legend, if \code{legend} is \code{TRUE}.
}
  \item{legend.lab}{
  legend labels, if \code{legend} is \code{TRUE}. If \code{NULL}, levels of the factor defined by \code{fac} are used.
}
  \item{pch}{
  symbol(s) used for points, when points are displayed (see \code{arrows}). If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{cex}{
  size of the points and/or of the variable names. For two-dimensional graphs: if \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group; otherwise a vector of any length can be defined, which is recycled if necessary. For dotcharts, gives the size used for points and all labels (see \code{\link{dotchart}}).
}
  \item{col}{
  color(s) used for points and/or variable names. If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary (not available for density histograms, see \code{dhist}).
}
  \item{lwd}{
  only used if arrows are displayed. Width of arrows. If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{drawintaxes}{
  logical indicating if internal axes should be drawn.
}
  \item{add}{
  only used for two-dimensional graphs. Logical indicating if the correlation circle should be added to an existing graph.
}
  \item{add.const}{
  only used for two-dimensional graphs and if \code{add} is \code{TRUE}. Constant by which correlations are multiplied to fit onto the original graph.
}
  \item{keepmar}{
  only used for two-dimensional graphs. Logical indicating if margins defined by MVA.corplot should be kept after plotting (necessary in some cases when \code{add=TRUE}).
}
}
\details{
This function should not be use directly. Prefer the general \code{\link[RVAideMemoire]{MVA.plot}}, to which all arguments can be passed.

Many multivariate analyses are supported, from various packages:

- PCA: \code{\link[ade4]{dudi.pca}}, \code{\link[vegan]{rda}}.

- sPCA: \code{\link[mixOmics]{spca}}.

- IPCA: \code{\link[mixOmics]{ipca}}.

- sIPCA: \code{\link[mixOmics]{sipca}}.

- LDA: \code{\link[MASS]{lda}}, \code{\link[ade4]{discrimin}}.

- PLS-DA (PLS2 on a dummy-coded factor): \code{\link[mixOmics]{plsda}}. X space only.

- sPLS-DA (sPLS2 on a dummy-coded factor): \code{\link[mixOmics]{splsda}}. X space only.

- CPPLS: \code{\link[pls]{mvr}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. X space only.

- PLSR: \code{\link[pls]{mvr}}, \code{\link[mixOmics]{pls}}, \code{\link[plsRglm]{plsR}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. X space only.

- sPLSR: \code{\link[mixOmics]{pls}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. X space only.

- PLS-GLR: \code{\link[plsRglm]{plsRglm}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set. Correlations are computed with Y on the link scale.

- PCR: \code{\link[pls]{mvr}}. Set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- CDA: \code{\link[ade4]{discrimin}}, \code{\link[ade4]{discrimin.coa}}.

- NSCOA: \code{\link[ade4]{dudi.nsc}}. For NSCOA there is no real correlation, but the classical representation of columns is arrows. This is why MVA.corplot was made able to deal with this analysis.

- CCA: \code{\link[vegan]{cca}}, \code{\link[ade4]{pcaiv}}. Constraints (only quantitative constraints are extracted) in constrained space only.

- Mix analysis: \code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{dudi.hillsmith}}. Only quantitative variables are displayed.

- RDA (or PCAIV): \code{\link[ade4]{pcaiv}}, \code{\link[ade4]{pcaivortho}}, \code{\link[vegan]{rda}}. With \code{\link[vegan]{rda}}, space 1 is constrained space, space 2 is unconstrained space. Only constrained space is available with \code{\link[ade4]{pcaiv}}, the opposite for \code{\link[ade4]{pcaivortho}}. Set 1 is constraints (only quantitative constraints are extracted), set 2 is dependent variables (only set 2 is available for \code{\link[ade4]{pcaivortho}}). If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- db-RDA: \code{\link[vegan]{capscale}}, \code{\link[vegan]{dbrda}}. Constraints (only quantitative constraints are extracted) in constrained space only.

- CCorA: \code{\link[vegan]{CCorA}}, \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y. With \code{\link[mixOmics]{rcc}} a third space is available, in which coordinates are means of X and Y coordinates. In this third space, set 1 is X, set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- rCCorA: \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates. In space 3, set 1 is X and set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- CIA: \code{\link[ade4]{coinertia}}. Space 1 is X, space 2 is Y, space 3 is a "common" space where X and Y scores are normed. In space 3, set 1 is X and set 2 is Y. If \code{set=12} in space 3 (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lws} can be defined differently for each set.

- PCIA: \code{\link[ade4]{procuste}}. Set 1 is X, set 2 is Y.

- 2B-PLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates. In space 3, set 1 is X and set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- 2B-sPLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y, space 3 is a "common" space in which coordinates are means of X and Y coordinates. In space 3, set 1 is X and set 2 is Y. If \code{set=12} (default), \code{fac} is not available and \code{pch},\code{cex}, \code{col}, \code{lwd} can be defined differently for each set.

- rGCCA: \code{\link[mixOmics]{wrapper.rgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).

- sGCCA: \code{\link[mixOmics]{wrapper.sgcca}}. Space can be 1 to n, the number of blocks (i.e. datasets).

- DIABLO: \code{\link[mixOmics]{block.plsda}}, \code{\link[mixOmics]{block.splsda}}. Space can be 1 to n, the number of blocks (i.e. datasets).
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
require(ade4)
data(olympic)
PCA <- dudi.pca(olympic$tab,scannf=FALSE)
MVA.plot(PCA,"corr")
}