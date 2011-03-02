\name{scat.mix.categorical}
\alias{scat.mix.categorical}
\title{
Representation des variables qualitatives en analyse mixte de Hill et Smith
}
\description{
Représente les variables qualitatives d'une analyse mixte de Hill et Smith à la manière d'une ACM.  La fonction est basée sur la fonction \code{s.class()} du package ade4.
}
\usage{
scat.mix.categorical(dudi.obj, xax = 1, yax = 2, csub = 2, possub = "topleft", ...)
}
\arguments{
  \item{dudi.obj}{
objet contenant les résultats de l'analyse, classiquement obtenue \emph{via} la fonction \code{dudi.mix()} du package ade4.
}
  \item{xax}{
axe à représenter horizontalement.
}
  \item{yax}{
axe à représenter verticalement.
}
  \item{csub}{
taille du titre de chaque graphe tracé (le titre étant le nom de la variable représentée).
}
  \item{possub}{
position du titre des graphes tracés (parmi \code{"topleft"}, \code{"topright"}, \code{"bottomleft"} et \code{"bottomright"})
}
  \item{\dots}{
arguments additionnels passés à la fonction \code{s.class()}. Voir l'aide de cette fonction.
}
}
\references{
Champely, S. 2005 Introduction à l'analyse multivariée (factorielle) sous R.
}
\author{
Maxime Hervé, d'après Stéphane Champely
}
\seealso{
\code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{s.class}}
}
\examples{
# Jeu de donnees bidon
age<-sample(15:60,50,replace=TRUE)
sexe<-sample(c("M","F"),50,replace=TRUE)
taille<-sample(155:190,50,replace=TRUE)
cheveux<-sample(c("Blond","Brun","Roux"),50,replace=TRUE)
yeux<-sample(c("Bleu","Vert","Marron"),50,replace=TRUE)
poids<-sample(50:85,50,replace=TRUE)
cote<-sample(c("Gaucher","Droitier"),50,replace=TRUE)
tab<-data.frame(age,sexe,taille,poids,cote,yeux,cheveux)
amix<-dudi.mix(tab,scannf=FALSE,nf=2)
scat.mix.categorical(amix)
}
