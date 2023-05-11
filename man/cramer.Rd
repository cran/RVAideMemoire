\name{cramer}
\alias{cramer}
\title{
Cramer's association coefficient
}
\description{
Computes the Cramer's association coefficient between 2 nominal variables.
}
\usage{
cramer(x, y)
}
\arguments{
  \item{x}{
a contingency table ('matrix' or 'table' object). \code{x} and \code{y} can also both be factors.
}
  \item{y}{
ignored if \code{x} is a contingency table. If not, \code{y} should be a vector of the same length.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
var1 <- sample(LETTERS[1:3],30,replace=TRUE)
var2 <- sample(letters[1:3],30,replace=TRUE)
cramer(var1,var2)
# or cramer(table(var1,var2))
}