\name{plot1comp.ind}
\alias{plot1comp.ind}
\title{
Plot of indiviuals and their class on a single axis (component)
}
\description{
Represent indivual points on a single axis (usually a factorial component) and add their class, respecting to a given factor.
}
\usage{
plot1comp.ind(dfxy, fac, axis = 1, col = 1:nlevels(fac), legend = TRUE,
  pos.legend = "topright", ...)
}
\arguments{
  \item{dfxy}{
a data frame containing coordinates of individuals on the axi(e)s.
}
  \item{fac}{
a factor giving the class of each individual.
}
  \item{axis}{
axis to be represented (the first by default).
}
  \item{col}{
a vector of colors (one value per class of the factor). Use numbers to display transparent colors.
}
  \item{legend}{
logical. If \code{TRUE} a legend is added to the graph.
}
  \item{pos.legend}{
position of the legend, if necessary. See \code{\link{legend}}.
}
  \item{...}{
further graphical arguments. See \code{\link{par}}.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[ade4]{s.class}}
}
\examples{
require(MASS)
data(iris)
LDA <- lda(iris[,1:4],iris$Species)
LDA.form <- LDA.format(LDA)
plot1comp.ind(LDA.form$li,LDA.form$grouping)
}