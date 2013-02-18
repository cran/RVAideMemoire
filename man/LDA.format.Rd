\name{LDA.format}
\alias{LDA.format}
\title{
Re-formating LDA data
}
\description{
Re-format data obtained from the \code{lda()} function of the package MASS, especially to get coordinates of individuals on factorial plans and correlation between original variables and LDA components.
}
\usage{
LDA.format(model)
}
\arguments{
  \item{model}{
object of class \code{"lda"} (from the function \code{lda()} of the package MASS).
}
}
\value{
  \item{x}{original data (X variables only).}
  \item{grouping}{class of each individual.}
  \item{li}{coordinates of each individual on the LDA components.}
  \item{co}{correlation between original variables and LDA components.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[MASS]{lda}}, \code{\link[ade4]{s.class}}, \code{\link[ade4]{s.corcircle}}
}
\examples{
require(MASS)
require(ade4)
data(iris)
model.LDA <- lda(iris[,1:4],iris$Species)
coord <- LDA.format(model.LDA)
s.class(coord$li,fac=coord$grouping,col=rainbow(nlevels(coord$grouping)),cellipse=0)
s.corcircle(coord$co,label=abbreviate(rownames(coord$co),3))
}