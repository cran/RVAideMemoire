\name{MVA.pairplot}
\alias{MVA.pairplot}
\title{
Paired plot of multivariate analyses
}
\description{
Displays a paired plot (\emph{i.e.} a score plot of paired points) of a multivariate analysis.
}
\usage{
MVA.pairplot(x, xax = 1, yax = 2, pairs = NULL, scaling = 2, space = 1, fac = NULL,
  xlab = NULL, ylab = NULL, main = NULL, ident = TRUE, labels = NULL, cex = 0.7, col = 1,
  lwd = 1, main.pos = c("bottomleft", "topleft", "bottomright", "topright"),
  main.cex = 1.3, legend = FALSE, legend.pos = c("topleft", "topright", "bottomleft",
  "bottomright"), legend.title = NULL, legend.lab = NULL, drawextaxes = TRUE,
  drawintaxes = TRUE, xlim = NULL, ylim = NULL)
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
  \item{pairs}{
  two-level factor identifying paired individuals (in the same order in both sets of points). Can be omitted with multivariate analyses where two sets of points are available in the same space (see \code{\link[RVAideMemoire]{MVA.scoreplot}}). In this case these sets are automatically detected.
}
  \item{scaling}{
  type of scaling. Only available with some analyses performed with the \code{vegan} package. See Details of \code{\link[RVAideMemoire]{MVA.scoreplot}}.
}
  \item{space}{
  scores to be displayed, when several spaces are available (see Details of \code{\link[RVAideMemoire]{MVA.scoreplot}}). \code{space} is the number of the space to be plotted.
}
  \item{fac}{
  an optional factor defining groups pairs.
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
  \item{ident}{
  logical indicating if variable names should be displayed.
}
  \item{labels}{
  names of the individuals. If \code{NULL} (default), labels correspond to row names of the data used in the multivariate analysis.
}
  \item{cex}{
  size of the labels.  If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{col}{
  color(s) used for arrows and labels. If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
}
  \item{lwd}{
 width of arrows. If \code{fac} is not \code{NULL}, can be a vector of length one or a vector giving one value per group. Otherwise a vector of any length can be defined, which is recycled if necessary.
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
  legend labels, if \code{legend} is \code{TRUE}. If \code{NULL} and \code{fac} is defined, levels of \code{fac} are used.
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
data(macaca)
PCIA <- procuste(macaca$xy1,macaca$xy2)
MVA.plot(PCIA,"pairs")
}