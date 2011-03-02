\name{least.rect}
\alias{least.rect}
\title{
Regression lineaire simple au sens des moindres rectangles
}
\description{
Renvoie un résumé d'une régression linéaire simple au sens des moindres rectangles, à la manière du résumé renvoyé par \code{summary(lm())} pour une régression au sens des moindres carrés.
}
\usage{
least.rect(x, y, conf.level = 0.95, theo = 1, coeff.arr = 4, p.arr = 4)
}
\arguments{
  \item{x}{
1ère variable de la régression.
}
  \item{y}{
2ème variable de la régression.
}
  \item{conf.level}{
intervalle de confiance des paramètres de la droite de régression.
}
  \item{theo}{
valeur théorique pour le test de conformité du coefficient directeur de la droite de régression.
}
  \item{coeff.arr}{
nombre de décimales affichées pour les coefficients et leur intervalle de confiance.
}
  \item{p.arr}{
nombre de décimales affichées pour les \emph{p-values} des tests réalisés.
}
}
\details{
Les noms \code{x} et \code{y} n'ont qu'une valeur graphique (\code{x} : variable destinée à être tracée en abscisses ; \code{y} : variable destinée à être tracée en ordonnées).  Ils ne font pas référence à une variable explicative et une à expliquer.

La fonction renvoie : l'équation de la droite de régression, l'intervalle de confiance des paramètres de la droite (coefficient directeur et ordonnée à l'origine), le résultat du test de conformité du coefficient directeur avec une valeur théorique choisie et enfin le coefficient de corrélation linéaire entre les deux variables, son intervalle de confiance et le résultat du test de significativité.
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{cor.test}}
}
\examples{
# Resume par defaut
x<-1:30+rnorm(30,0,3)
y<-1:30+rnorm(30,0,3)
least.rect(x,y)

# Conformite du coefficient directeur avec la valeur 0.5
# et intervalles de confiance a 99 pc
least.rect(x,y,conf.level=0.99,theo=0.5)
}
