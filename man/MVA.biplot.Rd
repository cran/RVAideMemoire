\name{MVA.biplot}
\alias{MVA.biplot}
\title{
Biplot of multivariate analyses
}
\description{
Displays a biplot of a multivariate analysis. This just consists in superimposing a score plot and a correlation circle (plus centroids of factor levels in constrained analyses, RDA or CCA). The correlation circle is adjusted to fit the size of the score plot.
}
\usage{
MVA.biplot(x, xax = 1, yax = 2, scaling = 2, sco.set = c(12, 1, 2),
  cor.set = c(12, 1, 2), space = 1, ratio = 0.9, weights = 1,
  constraints = c("nf", "n", "f"), sco.args = list(),
  cor.args = list(), f.col = 1, f.cex = 1)
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
  \item{scaling}{
  type of scaling (see \code{\link[RVAideMemoire]{MVA.scoreplot}}).
}
  \item{sco.set}{
  scores to be displayed, when several sets are available (see \code{\link[RVAideMemoire]{MVA.scoreplot}}).
}
  \item{cor.set}{
  correlations to be displayed, when several sets are available (see \code{\link[RVAideMemoire]{MVA.scoreplot}}).
}
  \item{space}{
  space to use, when several are available (see \code{\link[RVAideMemoire]{MVA.scoreplot}} and \code{\link[RVAideMemoire]{MVA.corplot}}).
}
  \item{ratio}{
  constant for adjustement of correlations to the size of the score plot (\code{0.9} means the longest arrows is 90\% of the corresponding axis).
}
  \item{weights}{
  only used with constrained analyses (RDA or CCA) where some constraints are factors. Individual weights, used to calculate barycenter positions.
}
  \item{constraints}{
  only used with constrained analyses (RDA or CCA). Type of constraints to display: quantitative (\code{"n"}), factors (\code{"f"}) or both (\code{"nf", default}).
}
  \item{sco.args}{
  list containing optional arguments to pass to \code{\link[RVAideMemoire]{MVA.scoreplot}}. All arguments are accepted.
}
  \item{cor.args}{
  list containing optional arguments to pass to \code{\link[RVAideMemoire]{MVA.corplot}}. All arguments are accepted except \code{xlab}, \code{ylab}, \code{circle}, \code{intcircle}, \code{drawintaxes}, \code{add} and \code{add.const}.
}
  \item{f.col}{
  color(s) used for barycenters in case of a constraint analysis (RDA or CCA) containing factor constraint(s). Can be a unique value, a vector giving one color per constraint or a vector giving one color per barycenter (all factors confounded).
}
  \item{f.cex}{
  size(s) used for barycenters in case of a constraint analysis (RDA or CCA) containing factor constraint(s). Can be a unique value, a vector giving one size per constraint or a vector giving one size per barycenter (all factors confounded).
}
}
\details{
This function should not be use directly. Prefer the general \code{\link[RVAideMemoire]{MVA.plot}}, to which all arguments can be passed.

All multivariate analyses covered by \code{\link[RVAideMemoire]{MVA.corplot}} can be used for biplots.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
require(vegan)
data(iris)
RDA <- rda(iris[,1:4]~Species,data=iris)
MVA.plot(RDA,"biplot",cor.args=list(col="purple"),ratio=0.8,f.col=c("red","green","blue"))
}