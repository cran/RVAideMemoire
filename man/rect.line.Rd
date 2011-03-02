\name{rect.line}
\alias{rect.line}
\title{
Droite de regression lineaire simple au sens des moindres rectangles
}
\description{
Permet de tracer une droite de régression linéaire simple au sens des moindres rectangles.
}
\usage{
rect.line(x, y)
}
\arguments{
  \item{x}{
1ère variable de la régression.
}
  \item{y}{
2ème variable de la régression.
}
}
\details{
Les noms \code{x} et \code{y} n'ont qu'une valeur graphique (\code{x} : variable destinée à être tracée en abscisses ; \code{y} : variable destinée à être tracée en ordonnées).  Ils ne font pas référence à une variable explicative et une à expliquer.
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{abline}}
}
\examples{
x<-1:30+rnorm(30,0,3)
y<-1:30+rnorm(30,0,3)
abline(rect.line(x,y))
}
