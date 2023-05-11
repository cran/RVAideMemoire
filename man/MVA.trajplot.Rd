\name{MVA.trajplot}
\alias{MVA.trajplot}
\title{
Trajectory plot of multivariate analyses
}
\description{
Displays a trajectory plot (\emph{i.e.} a score plot with trajectories linking defined points) of a multivariate analysis.
}
\usage{
MVA.trajplot(x, xax = 1, yax = 2, trajects, trajlab = NULL, scaling = 2,
  set = c(12, 1, 2), space = 1, xlab = NULL, ylab = NULL, main = NULL,
  pch = 16, cex = 1, trajlab.cex = 1, col = 1, lwd = 1, lty = 1,
  points = TRUE, allpoints = TRUE, arrows = TRUE, labels = NULL,
  main.pos = c("bottomleft", "topleft", "bottomright", "topright"),
  main.cex = 1.3, legend = FALSE, legend.pos = c("topleft", "topright",
  "bottomleft", "bottomright"), legend.title = NULL, legend.lab = NULL,
  legend.cex = 1, drawextaxes = TRUE, drawintaxes = TRUE, xlim = NULL,
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
  the vertical axis. Cannot be \code{NULL}, only two-dimensional graphs can be drawn.
}
  \item{trajects}{
  vector or list of vectors identifying trajectories. Each vector should give the number of the individuals to be linked, ordered from the first to the last one.
}
  \item{trajlab}{
  optional traject labels.
}
  \item{scaling}{
  type of scaling. Only available with some analyses performed with the \code{vegan} package. See Details of \code{\link[RVAideMemoire]{MVA.scoreplot}}.
}
  \item{set}{
  scores to be displayed, when several sets are available (see Details of \code{\link[RVAideMemoire]{MVA.scoreplot}}). \code{12} (default) for both sets, \code{1} for rows or X, \code{2} for columns or Y.
}
  \item{space}{
  scores to be displayed, when several spaces are available (see Details of \code{\link[RVAideMemoire]{MVA.scoreplot}}). \code{space} is the number of the space to be plotted.
}
  \item{xlab}{
  legend of the horizontal axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis.
}
  \item{ylab}{
  legend of the vertical axis. If \code{NULL} (default), automatic labels are used depending on the multivariate analysis.
}
  \item{main}{
  optional title of the graph.
}
  \item{pch}{
  symbols used for points. Can be a vector giving one value per trajectory (and a last one for non-linked points if \code{allpoints=TRUE}).
}
  \item{cex}{
  size of the labels. Can be a vector giving one value per trajectory (and a last one for non-linked points if \code{allpoints=TRUE}).
}
  \item{trajlab.cex}{
  size of trajectory labels. Can be a vector giving one value per trajectory.
}
  \item{col}{
  color(s) used for arrows and labels. If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{lwd}{
  width of trajectory segments. Can be a vector giving one value per trajectory.
}
  \item{lty}{
  type of trajectory segments. Can be a vector giving one value per trajectory.
}
  \item{points}{
  logical indicating if points should be displayed. If \code{FALSE}, points are replaced with their corresponding label (defined by \code{labels}).
}
  \item{allpoints}{
  logical indicating if points which do not belong to any trajectory should be drawn.
}
  \item{arrows}{
  logical indicating if trajectories should be oriented with arrows.
}
  \item{labels}{
  names of the individuals. If \code{NULL} (default), labels correspond to row names of the data used in the multivariate analysis.
}
  \item{main.pos}{
  position of the title, if \code{main} is not \code{NULL}. Default to \code{"bottomleft"}.
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
  legend labels, if \code{legend} is \code{TRUE}. If \code{NULL} and \code{trajlab} is defined, values of \code{trajlab} are used.
}
  \item{legend.cex}{
  size of legend labels, if \code{legend} is \code{TRUE}.
}
  \item{drawextaxes}{
  logical indicating if external axes should be drawn..
}
  \item{drawintaxes}{
  logical indicating if internal axes should be drawn.
}
  \item{xlim}{
  limits of the horizontal axis. If \code{NULL}, limits are computed automatically.
}
  \item{ylim}{
  limits of the vertical axis. If \code{NULL}, limits are computed automatically.
}
}
\details{
This function should not be use directly. Prefer the general \code{\link[RVAideMemoire]{MVA.plot}}, to which all arguments can be passed.

All multivariate analyses supported by \code{\link[RVAideMemoire]{MVA.scoreplot}} can be used for a paired plot.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
require(ade4)
data(olympic)
PCA <- dudi.pca(olympic$tab,scannf=FALSE)
MVA.plot(PCA,"traject",trajects=list(1:10,25:30),col=c(2,3,1),trajlab=c("T1","T2"))
}