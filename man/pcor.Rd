\name{pcor}
\alias{pcor}
\title{
(Semi-)Partial correlation
}
\description{
Computes the (semi-)partial correlation of \code{x} and \code{y}, controlling for \code{z}.
}
\usage{
pcor(x, y, z, semi = FALSE, use = "complete.obs", method = c("pearson",
  "spearman"))
}
\arguments{
  \item{x}{
a numeric vector.
}
  \item{y}{
a numeric vector.
}
  \item{z}{
a numeric vector, matrix, data frame or list giving the controlling variables. For matrices, variables must be placed in columns.
}
  \item{semi}{
logical. If \code{TRUE} the semi-partial correlation coefficient is computed. In that case only \code{y} is controlled for \code{z}.
}
  \item{use}{
same as \code{use} of \code{\link{cor}}.
}
  \item{method}{
same as \code{method} of \code{\link{cor}}.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{pcor.test}} for confidence intervals (and tests).
}
\examples{
set.seed(1444)
x <- 1:30
y <- 1:30+rnorm(30,0,2)
z1 <- runif(30,0,4)
z2 <- 30:1+rnorm(30,0,3)
pcor(x,y,z1)
pcor(x,y,list(z1,z2))
}