\name{cor.2comp}
\alias{cor.2comp}
\title{
Comparaison de 2 coefficients de correlation lineaire de Pearson
}
\description{
Réalise le test de comparaison de 2 coefficients de corrélation linéaire de Pearson.  Si la différence n'est pas significative, la fonction renvoie la valeur du coefficient de corrélation commun, son intervalle de confiance et le résultat du test de conformité avec une valeur théorique choisie.
}
\usage{
cor.2comp(var1, var2, var3, var4, alpha = 0.05, conf.level = 0.95, r.arr = 4, theo = 0, p.arr = 4)
}
\arguments{
  \item{var1}{
1ère variable définissant la 1ère corrélation.
}
  \item{var2}{
2ème variable définissant la 1ère corrélation.
}
  \item{var3}{
1ère variable définissant la 2ème corrélation.
}
  \item{var4}{
2ème variable définissant la 2ème corrélation.
}
  \item{alpha}{
seuil de significativité du test de comparaison des 2 coefficients de corrélation.
}
  \item{conf.level}{
intervalle de confiance du coefficient de corrélation commun.
}
  \item{r.arr}{
nombre de décimales affichées des coefficients de corrélation.
}
  \item{theo}{
valeur théorique pour le test de conformité du coefficient de corrélation commun.
}
  \item{p.arr}{
nombre de décimales affichées des \emph{p-values} des tests réalisés.
}
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{cor.test}}
}
\examples{
# Test par defaut
cor1.var1<-1:30+rnorm(30,0,2)
cor1.var2<-1:30+rnorm(30,0,3)
cor2.var1<--1:-30+rnorm(30,0,2)
cor2.var2<--1:-30+rnorm(30,0,3)
cor.2comp(cor1.var1,cor1.var2,cor2.var1,cor2.var2)

# Seuil de significativite a 3 pc et conformite du coefficient de correlation commun
# avec la valeur 0.5
 cor.2comp(cor1.var1,cor1.var2,cor2.var1,cor2.var2,alpha=0.03,theo=0.5)
}
