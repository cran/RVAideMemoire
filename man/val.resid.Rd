\name{val.resid}
\alias{val.resid}
\title{
Analyse graphique des residus d'un modele lineaire ou lineaire generalise
}
\description{
Permet d'observer graphiquement si les résidus d'un modèle repsectent les conditions d'équivariance et de normalité de distribution.
}
\usage{
val.resid(model, reg=FALSE, fact=NULL)
}
\arguments{
  \item{model}{
modèle, de type \code{lm()} ou \code{glm()}.
}
  \item{reg}{
logique : \code{TRUE} si le modèle est celui d'une régression, \code{FALSE} si c'est celui d'une analyse de variance/déviance.
}
  \item{fact}{
si le modèle est celui d'une analyse de variance/déviance, nom du facteur pour lequel on veut tracer le diagramme en boîtes à moustaches (entre guillemets).
}
}
\details{
Les résidus sont obtenus \emph{via} la fonction \code{residuals()} si le modèle est linéaire ou linéaire généralisé avec une loi continue (\code{gaussian}, \code{inverse.gaussian} ou \code{Gamma}), \emph{via} la fonction \code{qresiduals()} du package statmod si le modèle est linéaire généralisé avec une loi discontinue (\code{poisson}, \code{binomial}, \code{quasipoisson}, \code{quasibinomial} ou binomiale négative \emph{via} la fonction \code{glm.nb()} du package MASS).
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link{residuals}}, \code{\link[statmod]{qresiduals}}, \code{\link{qqnorm}}, \code{\link{boxplot}}
}
\examples{
# Regression
x<-1:30
y<-1:30+rnorm(30,0,2)
modele<-lm(y~x)
val.resid(modele,reg=TRUE)

# Analyse de variance
f<-factor(rep(LETTERS[1:3],each=10))
modele2<-lm(y~f)
val.resid(modele2,reg=FALSE,fact="f")
}
