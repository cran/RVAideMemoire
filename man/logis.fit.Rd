\name{logis.fit}
\alias{logis.fit}
\title{
Ajustement graphique d'une regression logistique binaire simple aux donnees intitiales
}
\description{
Découpe les valeurs de la variable explicative en un certains nombre d'intervalles, et calcule la probabilité de réponse et son erreur standard pour chaque intervalle. Les points et barres d'erreurs correspondants sont ensuite ajoutés sur le graphe qui doit avoir été préalablement tracé.
}
\usage{
logis.fit(model, int=5, ...)
}
\arguments{
  \item{model}{
modèle de la régression, de la forme \code{glm(y~x,family=binomial)}.
}
  \item{int}{
nombre d'intervalles à créer (= nombre de points à tracer).
}
  \item{\dots}{
arguments additionnels passés aux fonctions \code{points()} et \code{segments()}. Voir l'aide de ces fonctions.
}
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{glm}}, \code{\link{points}}, \code{\link{segments}}
}
\examples{
x<-1:50
y<-c(rep(0,15),sample(0:1,20,replace=TRUE),rep(1,15))
modele<-glm(y~x,family=binomial)
plot(x,y)
lines(x,modele$fitted.values)
logis.fit(modele)
}
