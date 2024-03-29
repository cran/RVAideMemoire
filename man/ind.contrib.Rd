\name{ind.contrib}
\alias{ind.contrib}
\title{
Individual contributions in regression
}
\description{
Computes difference in regression parameters when each individual is dropped, expressed in proportion of the whole regression coefficients. The function deals with \code{\link{lm}} (including \link{glm}) and \code{\link[RVAideMemoire]{least.rect}} models.
}
\usage{
ind.contrib(model, print.diff = FALSE, graph = TRUE, warning=25)
}
\arguments{
  \item{model}{
model (of class \code{"lm"} or \code{"least.rect"}).
}
  \item{print.diff}{
logical. If \code{TRUE} results are printed.
}
  \item{graph}{
logical. If \code{TRUE} results are returned in a graphical way.
}
  \item{warning}{
level of graphical warning.
}
}
\value{
  \item{coefficients}{coefficients of each computed regression.}
  \item{coefficients.diff}{difference in coefficients between each computed regression and the whole regression.}
  \item{coefficients.prop}{difference in coefficients expressed in proportion of the whole regression coefficients.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{lm.influence}}, \code{\link[RVAideMemoire]{least.rect}}
}
\examples{
x <- 1:30
y <- 1:30+rnorm(30,0,4)
model1 <- lm(y~x)
model2 <- least.rect(y~x)
ind.contrib(model1)
ind.contrib(model2)
}