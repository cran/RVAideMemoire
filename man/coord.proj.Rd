\name{coord.proj}
\alias{coord.proj}
\title{
Coordinates of projected points
}
\description{
Returns the coordinates of a set of points when orthogonally projected on a new axis.
}
\usage{
coord.proj(coord,slp)
}
\arguments{
  \item{coord}{
2-column data frame or matrix giving the original coordinates (left column: x, right column: y).
}
  \item{slp}{
slope of the new axis.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
data(iris)

# Original coordinates
plot(Petal.Length~Sepal.Length,pch=16,col=as.numeric(iris$Species),data=iris)

# New axis
abline(-6,1.6)

# Coordinates on new axis
new.coord <- coord.proj(iris[,c("Sepal.Length","Petal.Length")],1.6)
stripchart(new.coord~Species,data=iris,col=1:3)
}