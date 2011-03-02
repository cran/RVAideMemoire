\name{reg.ordcomp}
\alias{reg.ordcomp}
\title{
Comparaison des ordonnees a l'origine de plusieurs droites de regression lineaire simple au sens des moindres carres et coefficient directeur commun
}
\description{
Dans le cadre de plusieurs droites de régression linéaire parallèles, la fonction renvoie le coefficient directeur commun à toutes les droites et son intervalle de confiance.  Elle renvoie également les ordonnées à l'origine de chaque droite ainsi que leur intervalle de confiance et le résultat du test de conformité avec une valeur théorique choisie.  Elle réalise enfin toutes les comparaisons deux-à-deux possibles entre les ordonnées à l'origine.
}
\usage{
reg.ordcomp(var, covar, fact, conf.level = 0.95, coeff.arr = 4, theo = rep(0, nlevels(fact)), p.method = "fdr", p.arr = 4)
}
\arguments{
  \item{var}{
variable à expliquer.
}
  \item{covar}{
variable explicative (covariable dans le cadre d'une ANCOVA).
}
  \item{fact}{
facteur définissant les régression à comparer.
}
  \item{conf.level}{
intervalle de confiance du coefficient directeur commun et des ordonnées à l'origine des droites de régression.
}
  \item{coeff.arr}{
nombre de décimales affichées des paramètres des droites et de leur intervalle de confiance.
}
  \item{theo}{
valeurs théoriques des ordonnées à l'origine.
}
  \item{p.method}{
méthode de correction des \emph{p-values} pour les comparaisons multiples. Voir l'aide de la fonction \code{p.adjust()}.
}
  \item{p.arr}{
nombre de décimales affichées des \emph{p-values} des tests réalisés.
}
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{lm}}, \code{\link{residuals}}, \code{\link[gtools]{combinations}}, \code{\link{p.adjust}}
}
\examples{
# Tests par defaut
covar<-rep(1:30,3)
var<-c(1:30+rnorm(30,0,3),seq(10,35,25/29)+rnorm(30,0,3),seq(-27,0,27/29)+rnorm(30,0,3))
facteur<-as.factor(rep(LETTERS[1:3],each=30))
reg.ordcomp(var,covar,facteur)

# Intervalles de confiance a 99 pc, ordonnees a l'origine theoriques non nulles
# et correction des p-values par la methode de Bonferroni
reg.ordcomp(var,covar,facteur,conf.level=0.99,theo=c(1,8,-20),p.method="bonferroni")
}
