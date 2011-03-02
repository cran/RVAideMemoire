\name{reg.ci}
\alias{reg.ci}
\title{
Intervalles de confiance d'une regression lineaire simple au sens des moindres carres
}
\description{
Trace l'intervalle de confiance des moyennes conditionnelles ou des valeurs individuelles d'une régression au sens des moindres carrés.
}
\usage{
reg.ci(model, conf.level = 0.95, type = c("moy", "ind"), ...)
}
\arguments{
  \item{model}{
modèle de la régression, de la forme \code{lm(y~x)}.
}
  \item{conf.level}{
niveau de confiance souhaité.
}
  \item{type}{
type d'intervalle de confiance souhaité (\code{"moy"} pour les moyennes conditionnelles, \code{"ind"} pour les valeurs individuelles).
}
  \item{\dots}{
autres arguments graphiques (couleur du trait, type de trait\dots).
}
}
\details{
L'intervalle de confiance tracé par défaut est celui des moyennes conditionelles.
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{lm}}, \code{\link{predict}}, \code{\link{residuals}}, \code{\link{lines}}
}
\examples{
# Intervalle de confiance des moyennes conditionnelles
x<-1:50
y<-1:50+rnorm(50,0,4)
regression<-lm(y~x)
plot(x,y)
abline(regression)
reg.ci(regression,type="moy",col="red")

# Intervalle de confiance a 99 pc des valeurs individuelles
reg.ci(regression,conf.level=0.99,type="ind",col="blue",lty=2)
}
