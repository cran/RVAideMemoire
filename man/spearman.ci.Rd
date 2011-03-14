\name{spearman.ci}
\alias{spearman.ci}
\title{
Intervalle de confiance d'un coefficient de correlation lineaire de Spearman
}
\description{
Renvoie l'intervalle de confiance d'un coefficient de corrélation linéaire de Spearman.
}
\usage{
spearman.ci(var1, var2, rep = 1000, conf.level = 0.95, coeff.arr = 4)
}
\arguments{
  \item{var1}{
1ère variable à corréler.
}
  \item{var2}{
2ème variable à corréler.
}
  \item{rep}{
nombre de répétitions du bootstrap.
}
  \item{conf.level}{
intervalle de confiance.
}
  \item{coeff.arr}{
nombre de décimales affichées du coefficient de corrélation et de son intervalle de confiance.
}
}
\details{
L'intervalle de confiance est calculé à partir d'un bootstrap via la fonction \code{boot} du package boot.
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{cor.test}}, \code{\link[boot]{boot}}
}
\examples{
# Intervalle de confiance par defaut
var1<-sample(1:50,15,replace=TRUE)
var2<-sample(1:50,15,replace=TRUE)
spearman.ci(var1,var2)

# Bootstrap a 100 repetitions et intervalle de confiance a 99 pc
spearman.ci(var1,var2,rep=100,conf.level=0.99)
}
