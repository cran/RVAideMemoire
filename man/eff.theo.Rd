\name{eff.theo}
\alias{eff.theo}
\title{
Conformite de plusieurs proportions avec des valeurs theoriques - effectifs theoriques
}
\description{
Renvoie un tableau de contingence des effectifs théoriques dans le cadre d'un test de comparaison de plusieurs proportions avec des valeurs théoriques.
}
\usage{
eff.theo(data, p.theo, graphe = FALSE)
}
\arguments{
  \item{data}{
tableau de contingence des effectifs testés.
}
  \item{p.theo}{
proportions théoriques.
}
  \item{graphe}{
logique : si \code{TRUE} le graphe représentant le tableau de contingence des effectifs théoriques est tracé.
}
}
\details{
Le tableau de contingence des effectifs testés doit avoir en lignes les populations (statistiques) à comparer et en colonnes les deux classess de la variable définissant les proportions.  La colonne de gauche est utilisée pour calculer les proportions testées.

Le tableau de contingence des effectifs théoriques a la même structure que le tableau des effectifs testés.

La fonction renvoie le nombre de cases du tableau des effectifs théoriques qui peuvent avoir une valeur inférieure ou égale à 5 pour que la règle de Cochran soit respectée.  Cette règle dit qu'au moins 80 \% des cases doivent avoir un effectif théorique supérieur ou égal à 5.
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{prop.test}}, \code{\link{chisq.test}}, \code{\link{mosaicplot}}
}
\examples{
# Pas de graphe trace (fonctionnement par defaut)
pop<-sample(LETTERS[1:3],60,replace=TRUE)
prop<-sample(c(0,1),60,replace=TRUE)
tab.cont<-table(pop,prop)
prop.theo<-c(0.2,0.5,0.7)
eff.theo(tab.cont,p.theo=prop.theo)

# Avec une representation graphique du tableau des effectifs theoriques
eff.theo(tab.cont,p.theo=prop.theo,graphe=TRUE)
}
