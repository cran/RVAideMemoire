\name{se}
\alias{se}
\title{
Erreur standard de la moyenne
}
\description{
Renvoie l'erreur standard de la moyenne d'une série de données.
}
\usage{
se(var)
}
\arguments{
  \item{var}{
série de données.
}
}
\details{
La fonction gère par défaut les données manquantes.
}
\author{
Maxime Hervé
}
\examples{
serie<-sample(1:50,30)
se(serie)
}
