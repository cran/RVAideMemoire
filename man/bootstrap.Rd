\name{bootstrap}
\alias{bootstrap}
\title{
  Bootstrap
}
\description{
Version simplifiée de la fonction \code{boot()} du package boot.
}
\usage{
bootstrap(vect, fun, rep = 1000, conf.level = 0.95, arr = 4, ...)
}
\arguments{
  \item{vect}{
série de données à partir de laquelle réaliser le calcul.
}
  \item{fun}{
fonction réalisant le calcul souhaité. Doit être de la forme \code{function(x,i) ...(x[i])}.
}
  \item{rep}{
nombre de répétitions du bootstrap.
}
  \item{conf.level}{
intervalle de confiance souhaité
}
  \item{arr}{
nombre de décimales affichées de l'intervalle de confiance renvoyé.
}
  \item{\dots}{
arguments additionnels passés à la fonction \code{boot()}. Voir l'aide de cette fonction.
}
}
\details{
L'argument utilisé dans l'expression à calculer doit être le même que le premier déclaré à \code{function} (ici \code{x}).  Cet argument doit être suivi du second déclaré à \code{function()} placé entre crochets. Celui-ci est par défaut \code{i} (voir l'aide de la fonction \code{boot()}).
}
\author{
Maxime Hervé
}
\seealso{
\code{\link[boot]{boot}}
}
\examples{
# Intervalle de confiance a 95 pc d'une moyenne sur petit echantillon
ech<-sample(1:50,10,replace=TRUE)
bootstrap(ech,function(x,i) mean(x[i]))

# La meme chose avec un intervalle de confiance a 99 pc
bootstrap(ech,function(x,i) mean(x[i]),conf.level=0.99)

# Un calcul (un peu) plux complexe, l'intervalle de confiance de l'erreur standard
# de la moyenne
bootstrap(ech,function(x,i) sd(x[i])/sqrt(length(x[i])))
}
