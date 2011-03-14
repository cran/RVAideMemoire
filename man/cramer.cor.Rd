\name{cramer.cor}
\alias{cramer.cor}
\title{
Coefficient d'association de Cramer
}
\description{
Renvoie le coefficient d'association de Cramer de deux variables qualitatives, ainsi que son intervalle de confiance.
}
\usage{
cramer.cor(var1, var2, rep = 1000, conf.level = 0.95, coeff.arr = 4)
}
\arguments{
  \item{var1}{
1ère variable.
}
  \item{var2}{
2ème variable.
}
  \item{rep}{
nombre de répétitions du bootstrap pour le calcul de l'intervalle de confiance.
}
  \item{conf.level}{
intervalle de confiance du coefficient d'association.
}
  \item{coeff.arr}{
nombre de décimales affichées du coefficient d'association et de son intervalle de confiance.
}
}
\details{
L'intervalle de confiance est calculé à partir d'un bootstrap via la fonction \code{boot} du package boot.
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{chisq.test}}, \code{\link[boot]{boot}}
}
\examples{
# Calcul par defaut
var1<-sample(LETTERS[1:3],15,replace=TRUE)
var2<-sample(letters[1:3],15,replace=TRUE)
cramer.cor(var1,var2)

# Bootstrap a 100 repetitions et intervalle de confiance a 99 pc
cramer.cor(var1,var2,rep=100,conf.level=0.99)
}
