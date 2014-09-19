\name{scatter.coa2}
\alias{scatter.coa2}
\title{
Plot of the factorial maps for a correspondence analysis
}
\description{
Performs the scatter diagrams of a correspondence analysis. The function is a copy of \code{\link[ade4]{scatter.coa}} including 2 new arguments: \code{sep} and \code{boxes}.
}
\usage{
scatter.coa2(x, xax = 1, yax = 2, sep = TRUE, boxes = FALSE,
  method = 1:3, clab.row = 0.75, clab.col = 1.25, posieig = "none",
  sub = NULL, csub = 2, ...)
}
\arguments{
  \item{x}{
an object of class \code{coa}.
}
  \item{xax}{
the column number for the x-axis (horizontal).
}
  \item{yax}{
the column number for the y-axis (vertical).
}
  \item{sep}{
a logical value indicating if score plots of rows and columns should be plotted separately (default) or not. If \code{TRUE}, the sale scale is applied to both plots.
}
  \item{boxes}{
a logical value indicating whether a box should be drawn around labels.
}
  \item{method}{
an integer between 1 and 3. 1: rows and columns with the coordinates of lambda variance. 2: rows variance 1 and columns by averaging. 3: columns variance 1 and rows by averaging.
}
  \item{clab.row}{
a character size for the rows.
}
  \item{clab.col}{
a character size for the columns. If \code{sep} is \code{TRUE}, size given by \code{clab.row} is applied in both plots.
}
  \item{posieig}{
if \code{"top"} the eigenvalues bar plot is upside, if \code{"bottom"} it is downside, if \code{"none"} no plot.
}
  \item{sub}{
a string of characters to be inserted as legend.
}
  \item{csub}{
a character size for the legend, used with \code{par("cex")*csub}.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}

\author{
Maxime Hervé <mx.herve@gmail.com>, based on a function of Daniel Chessel
}
\seealso{
\code{\link[ade4]{scatter.coa}}
}
\examples{
require(ade4)
data(housetasks)

coa <- dudi.coa(housetasks,scannf=FALSE,nf=2)
dev.new(height=7,width=14)
scatter.coa2(coa)
}
