\name{plot1comp.ind}
\alias{plot1comp.ind}
\title{
Plot of indiviuals and their class on a single axis (component)
}
\description{
Represent indivual points on a single axis (usually a factorial component) and add their class, respecting to a given factor.
}
\usage{
plot1comp.ind(dfxy, fac, axis = 1, col = 1:nlevels(fac), cex = 1, box = TRUE)
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
a vector of colors (one value per class of the factor).
}
  \item{cex}{
size of class names.
}
  \item{box}{
logical, should a box to be drawn around class names?
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
plot1comp.ind(LDA.form$li,LDA.form$grouping,col=1:3)
}