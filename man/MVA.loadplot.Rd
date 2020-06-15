\name{MVA.loadplot}
\alias{MVA.loadplot}
\title{
Loading plot of multivariate analyses
}
\description{
Displays a loading plot of a multivariate analysis.
}
\usage{
MVA.loadplot(x, xax = 1, yax = 2, fac = NULL, set = c(12, 1, 2), space = 1, map = TRUE,
  xlab = NULL, ylab = NULL, main = NULL, points = TRUE, ident = TRUE, links = TRUE, 
  line = TRUE, labels = NULL, main.pos = c("bottomleft", "topleft","bottomright",
  "topright"), main.cex = 1.3, legend = FALSE, legend.pos = c("topleft", "topright",
  "bottomleft", "bottomright"), legend.title = NULL, legend.lab = NULL, pch = 16,
  cex = 1, col = 1, lwd = 1, lty = 1, drawextaxes = TRUE, drawintaxes = TRUE, xlim = NULL,
  ylim = NULL)
}
\arguments{
  \item{x}{
  a multivariate analysis (see Details).
}
  \item{xax}{
  the horizontal axis.
}
  \item{yax}{
  the vertical axis. This can be set to \code{NULL} for a one-dimensional graph.
}
  \item{fac}{
  only used for one-dimensional graphs. An optional factor defining groups of variables.
}
  \item{set}{
  variables to be displayed, when several sets are available (see Details). \code{12} (default) for both sets, \code{1} for X, \code{2} for Y.
}
  \item{space}{
  variables to be displayed, when several spaces are available (see Details). \code{space} is the number of the space to be plotted.
}
  \item{map}{
  logical indicating if a two-dimensional (\code{TRUE}, default) or a one-dimensional graph should be drawn. A one-dimensional graph can show loadings for one or two dimensions, both horizontally.
}
  \item{xlab}{
  only used for two-dimensional graphs. Legend of the horizontal axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis.
}
  \item{ylab}{
  legend of the vertical axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis.
}
  \item{main}{
  optional title of the graph.
}
  \item{points}{
  only used for two-dimensional graphs. If \code{FALSE}, lines or points (see \code{links}) are replaced with their corresponding label (defined by \code{labels}).
}
  \item{ident}{
  logical indicating if variable names should be displayed. Only used when \code{points=TRUE} for two-dimensional graphs.
}
  \item{links}{
  only used for two-dimensional graphs when \code{points=TRUE}. Logical indicating if variables should be linked to the origin of the graph. If \code{FALSE}, points are displayed at the extremity of the segments.
}
  \item{line}{
  only used for one-dimensional graphs when \code{yax=NULL}. Logical indicating if loadings should be linked (default) as displayed as sticks.
}
  \item{labels}{
  only used if \code{ident=TRUE}. Names of the variables. If \code{NULL} (default), labels correspond to variable names found in the data used in the multivariate analysis.
}
  \item{main.pos}{
  only used for one-dimensional graphs. Position of the title, if \code{main} is not \code{NULL}. Default to \code{"bottomleft"}.
}
  \item{main.cex}{
  size of the title, if \code{main} is not \code{NULL}.
}
  \item{legend}{
  logical indicating if a legend should be added to the graph.
}
  \item{legend.pos}{
  position of the legend, if \code{legend} is \code{TRUE}. Default to \code{"topleft"}.
}
  \item{legend.title}{
  optional title of the legend, if \code{legend} is \code{TRUE}.
}
  \item{legend.lab}{
  legend labels, if \code{legend} is \code{TRUE}. If \code{NULL} for a one-dimensional graph, dimension names are used. If \code{NULL} for a two-dimensional graph, levels of the factor defined by \code{fac} are used.
}
  \item{pch}{
  only used for two-dimensional graphs. Symbol(s) used for points, when points are displayed (see \code{links}). If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{cex}{
  size of the points and/or of the variable names. For two-dimensional graphs: if \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group; otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{col}{
  color(s) used for points, variable names and/or lines/sticks. For one-dimensional graphs, can be a vector of length one or a vector giving one value per line. For two-dimensional graphs: if \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary (not available for density histograms, see \code{dhist}).
}
  \item{lwd}{
  width of lines. For one-dimensional graphs, can be a vector of length one or a vector giving one value per line. For two-dimensional graphs: if \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{lty}{
  only used for one-dimensional graphs. Can be a vector of length one or a vector giving one value per line.
}
  \item{drawextaxes}{
  logical indicating if external axes should be drawn.
}
  \item{drawintaxes}{
  only used for two-dimensional graphs. Logical indicating if internal axes should be drawn.
}
  \item{xlim}{
  only used in two-dimensional graphs. Limits of the horizontal axis. If \code{NULL}, limits are computed automatically.
}
  \item{ylim}{
  limits of the vertical axis. If \code{NULL}, limits are computed automatically.
}
}
\details{
This function should not be use directly. Prefer the general \code{\link[RVAideMemoire]{MVA.plot}}, to which all arguments can be passed.

Many multivariate analyses are supported, from various packages:

- PCA: \code{\link{prcomp}}, \code{\link{princomp}}, \code{\link[ade4]{dudi.pca}}, \code{\link[vegan:cca]{rda}}, \code{\link[mixOmics]{pca}}, \code{\link[labdsv]{pca}}.

- sPCA: \code{\link[mixOmics]{spca}}.

- IPCA: \code{\link[mixOmics]{ipca}}.

- sIPCA: \code{\link[mixOmics]{sipca}}.

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

- PCIA: \code{\link[ade4]{procuste}}. Set 1 is X, set 2 is Y.

- RDA (or PCAIV): \code{\link[ade4]{pcaiv}}, \code{\link[ade4]{pcaivortho}}, \code{\link[vegan:cca]{rda}}. With \code{\link[vegan:cca]{rda}}, space 1 is constrained space, space 2 is unconstrained space. Only constrained space is available with \code{\link[ade4]{pcaiv}}, the opposite for \code{\link[ade4]{pcaivortho}}.

- CCorA: \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y.

- rCCorA: \code{\link[mixOmics]{rcc}}. Space 1 is X, space 2 is Y.

- CIA: \code{\link[ade4]{coinertia}}. Space 1 is X, space 2 is Y.

- 2B-PLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y.

- 2B-sPLS: \code{\link[mixOmics]{pls}}. Space 1 is X, space 2 is Y.

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
MVA.plot(PCA,"load")
}