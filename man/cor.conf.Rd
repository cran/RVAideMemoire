\name{cor.conf}
\alias{cor.conf}
\title{
Conformite d'un coefficient de correlation lineaire de Pearson avec une valeur theorique
}
\description{
Réalise le test de conformité d'un coefficient de corrélation linéaire de Pearson avec une valeur théorique choisie.
}
\usage{
cor.conf(var1, var2, theo, r.arr = 4, p.arr = 4)
}
\arguments{
  \item{var1}{
1ère variable à corréler.
}
  \item{var2}{
2ème variable à corréler.
}
  \item{theo}{
valeur théorique.
}
  \item{r.arr}{
nombre de décimales affichées des coefficients de corrélation.
}
  \item{p.arr}{
nombre de décimales affichées de la \emph{p-value} du test.
}
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{cor.test}}
}
\examples{
var1<-1:30+rnorm(30,0,4)
var2<-1:30+rnorm(30,0,4)
cor.conf(var1,var2,theo=0.8)
}
