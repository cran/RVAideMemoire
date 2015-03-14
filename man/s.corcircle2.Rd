\name{s.corcircle2}
\alias{s.corcircle2}
\title{
Plot of the factorial maps of a correlation circle
}
\description{
Performs the scatter diagram of a correlation circle. The function is a copy of \code{\link[ade4]{s.corcircle}} including 6 new arguments: \code{boxes}, \code{arrows}, \code{col}, \code{lwd} and \code{cpoint}.
}
\usage{
s.corcircle2(dfxy, xax = 1, yax = 2, label = row.names(df), 
  clabel = 1, grid = TRUE, sub = "", csub = 1, possub = "bottomleft",
  cgrid = 0, fullcircle = TRUE, box = FALSE, add.plot = FALSE,
  circle = TRUE, boxes = FALSE, arrows = TRUE, col = "black", lwd = 1,
  cpoint = 0.8)
}
\arguments{
  \item{dfxy}{
a data frame with two coordinates.
}
  \item{xax}{
the column number for the x-axis (horizontal).
}
  \item{yax}{
the column number for the y-axis (vertical).
}
  \item{label}{
a vector of strings of characters for the point labels.
}
  \item{clabel}{
if not NULL, a character size for the labels, used with \code{par("cex")*clabel}. Can be a unique value or one value per label.
}
  \item{grid}{
a logical value indicating whether a grid in the background of the plot should be drawn.
}
  \item{sub}{
a string of characters to be inserted as legend.
}
  \item{csub}{
a character size for the legend, used with \code{par("cex")*csub}.
}
  \item{possub}{
a string of characters indicating the sub-title position (\code{"topleft"}, \code{"topright"}, \code{"bottomleft"}, \code{"bottomright"}).
}
  \item{cgrid}{
a character size, parameter used with \code{par("cex")*cgrid} to indicate the mesh of the grid.
}
  \item{fullcircle}{
a logical value indicating whether the complete circle sould be drawn.
}
  \item{box}{
a logical value indicating whether a box should be drawn.
}
  \item{add.plot}{
if TRUE uses the current graphics window.
}
  \item{circle}{
a logical value indicating whether the circle should be drawn.
}
  \item{boxes}{
a logical value indicating whether a box should be drawn around labels.
}
  \item{arrows}{
a logical value indicating whether arrows should be drawn (if \code{FALSE}, points are displayed).
}
  \item{col}{
a vector of colors (unique value or one value per arrow).
}
  \item{lwd}{
a vector of line widths (unique value or one value per arrow).
}
  \item{cpoint}{
a vector of point sizes (unique value or one value per arrow).
}
}

\author{
Maxime Hervé <mx.herve@gmail.com>, based on a function of Daniel Chessel
}
\seealso{
\code{\link[ade4]{s.corcircle}}
}
\examples{
require(ade4)
data(olympic)

pca <- dudi.pca(olympic$tab,scannf=FALSE,nf=2)
col <- c("red","red","blue","grey","red","red","blue","grey","blue","grey")
s.corcircle2(pca$co,col=col)
}
