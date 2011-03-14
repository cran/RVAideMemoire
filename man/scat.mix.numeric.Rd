\name{scat.mix.numeric}
\alias{scat.mix.numeric}
\title{
Representation des variables numeriques en analyse mixte de Hill et Smith
}
\description{
Représente les variables qualitatives d'une analyse mixte de Hill et Smith à la manière d'une ACP.  La fonction est basée sur la fonction \code{s.corcircle()} du package ade4.
}
\usage{
scat.mix.numeric(obj.dudi, xax = 1, yax = 2, ...)
}
\arguments{
  \item{obj.dudi}{
objet contenant les résultats de l'analyse, classiquement obtenue \emph{via} la fonction \code{dudi.pca()} du package ade4.
}
  \item{xax}{
axe à représenter horizontalement.
}
  \item{yax}{
axe à représenter verticalement.
}
  \item{\dots}{
arguments additionnels passés à la fonction \code{s.corcircle()}. Voir l'aide de cette fonction.
}
}
\author{
Stéphane Champely
}
\references{
Champely, S. 2005 Introduction à l'analyse multivariée (factorielle) sous R.
}
\seealso{
\code{\link[ade4]{dudi.pca}}, \code{\link[ade4]{s.corcircle}}
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
scat.mix.numeric(amix)
}
