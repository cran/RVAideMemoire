\name{reg.dircomp}
\alias{reg.dircomp}
\title{
Comparaison des coefficients directeurs de plusieurs droites de regression lineaire simple au sens des moindres carres
}
\description{
Renvoie la valeur et l'intervalle de confiance des coefficients directeurs des droites de régression et réalise toutes les comparaisons deux-à-deux possibles.
}
\usage{
reg.dircomp(var, covar, fact, conf.level = 0.95, coeff.arr = 4, p.method = "fdr", p.arr = 4)
}
\arguments{
  \item{var}{
variable à expliquer.
}
  \item{covar}{
variable explicative (covariable dans le cadre d'une ANCOVA).
}
  \item{fact}{
facteur définissant les régressions à comparer.
}
  \item{conf.level}{
intervalle de confiance des coefficients directeurs.
}
  \item{coeff.arr}{
nombre de décimales affichées des coefficients directeurs et de leur intervalle de confiance.
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
covar<-rep(1:30,3)
var<-c(seq(1,10,9/29)+rnorm(30,0,3),seq(1,30,1)+rnorm(30,0,3),seq(-1,-80,-79/29)+rnorm(30,0,3))
facteur<-as.factor(rep(LETTERS[1:3],each=30))
reg.dircomp(var,covar,facteur)
}
