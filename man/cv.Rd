\name{cv}
\alias{cv}
\title{
Coefficient de variation
}
\description{
Renvoie le coefficient de variation d'une série de données.
}
\usage{
cv(var, abs = TRUE, pc = TRUE)
}
\arguments{
  \item{var}{
série de données.
}
  \item{abs}{
logique : coefficient exprimé en valeur absolue ou non.
}
  \item{pc}{
logique : coefficient exprimé en pourcentage ou non (donc en proportion).
}
}
\author{
Maxime Hervé
}
\examples{
# Coefficient par defaut
serie<-sample(1:30,20)
cv(serie)

# Coefficient en proportion
cv(serie,pc=FALSE)
}
