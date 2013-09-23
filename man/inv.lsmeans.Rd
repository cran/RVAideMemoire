\name{inv.lsmeans}
\alias{inv.lsmeans}
\title{
LSMeans and SE on the scale of the response variable
}
\description{
Return LSmeans and their standard errors, obtained from \code{\link[lsmeans]{lsmeans}}, on the scale of the response variable.
}
\usage{
inv.lsmeans(model, lsm)
}
\arguments{
  \item{model}{
any model accepted by \code{\link[lsmeans]{lsmeans}}.
}
  \item{lsm}{
object returned by \code{\link[lsmeans]{lsmeans}}.
}
}
\value{
  \item{means}{vector of means.}
  \item{SE}{vector of standard errors.}
  \item{tab}{data frame containing means and standard erros.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[lsmeans]{lsmeans}}
}
\examples{
require(lsmeans)

# Example from the function glm()
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glm.D93 <- glm(counts~outcome+treatment,family="poisson")
lsm <- lsmeans(glm.D93,~outcome)
inv.lsmeans(glm.D93,lsm)
}
