\name{tend.curve}
\alias{tend.curve}
\title{
Courbe de tendance d'un nuage de points
}
\description{
Trace la courbe de tendance d'un nuage de points.  Cette fonction est basée sur la fonction \code{gam()} du package mgcv.
}
\usage{
tend.curve(x, y, ...)
}
\arguments{
  \item{x}{
variable tracée en abscisses.
}
  \item{y}{
variable tracée en ordonnées.
}
  \item{\dots}{
autres arguments graphiques (couleur du trait, type de trait\dots).
}
}
\author{
Maxime Hervé
}
\seealso{
\code{\link[mgcv]{gam}}, \code{\link{predict}}
}
\examples{
x<-1:30
y<-rexp(30,3)
plot(x,y)
tend.curve(x,y)
}
