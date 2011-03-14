\name{cook.dist}
\alias{cook.dist}
\title{
Graphe des distances de Cook individuelles dans une regression lineaire simple au sens des moindres carres
}
\description{
Version plus intuitive du graphe tracé par défaut par R.
}
\usage{
cook.dist(model, avert = 0.6)
}
\arguments{
  \item{model}{
modèle de la régression, de la forme \code{lm(y~x)}.
}
  \item{avert}{
seuil à partir duquel un avertissement graphique apparaît, en proportion du seuil théorique.
}
}
\details{
Lorsque que la distance de Cook d'un individu est proche du seuil théorique, une grande attention doit être porté à cet individu.  Il est conseillé d'analyser son influence en comparant le modèle à un second dans lequel l'individu en question a été retiré.
}
\author{
Maxime Hervé, d'après Gaël Millot
}
\seealso{
\code{\link{lm}}, \code{\link{ls.diag}}, \code{\link{lsfit}}
}
\examples{
# Graphe par defaut
x<-1:50
y<-1:50+rnorm(50,0,4)
regression<-lm(y~x)
cook.dist(regression)

# Avertissement quand la distance vaut 80 pc de la distance theorique
cook.dist(regression,avert=0.8)
}
