\name{cor.multcomp}
\alias{cor.multcomp}
\title{
Comparaison de plusieurs coefficients de correlation lineaire de Pearson
}
\description{
Réalise le test de comparaison de plusieurs coefficients de corrélation linéaire de Pearson.  S'il n'existe aucune différence significative, la fonction renvoie la valeur du coefficient de corrélation commun, son intervalle de confiance et réalise le test de conformité avec une valeur théorique choisie.  Dans le cas contraire, la fonction réalise toutes les comparaisons deux-à-deux possibles.
}
\usage{
cor.multcomp(var1, var2, fact, alpha = 0.05, conf.level = 0.95, r.arr = 4, theo = 0, p.method = "fdr", p.arr = 4)
}
\arguments{
  \item{var1}{
1ère variable à corréler.
}
  \item{var2}{
2ème variable à corréler.
}
  \item{fact}{
facteur définissant les groupes à comparer.
}
  \item{alpha}{
seuil de significativité du test global.
}
  \item{conf.level}{
intervalle de confiance du coefficient de corrélation commun.
}
  \item{r.arr}{
nombre de décimales affichées des coefficients de corrélation.
}
  \item{theo}{
valeur théorique du test de conformité du coefficient de corrélation commun.
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
\code{\link{cor.test}}, \code{\link[gtools]{combinations}}, \code{\link{p.adjust}}
}
\examples{
# Test par defaut
var1<-c(1:15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
var2<-c(-1:-15+rnorm(15,0,4),1:15+rnorm(15,0,1),1:15+rnorm(15,0,8))
facteur<-as.factor(rep(LETTERS[1:3],each=15))
cor.multcomp(var1,var2,facteur)

# Correction des p-values avec la methode de Bonferroni
cor.multcomp(var1,var2,facteur,p.method="bonferroni")
}