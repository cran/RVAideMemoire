\name{dummy}
\alias{dummy}
\title{
Dummy responses
}
\description{
Creates a matrix of dummy responses from a factor. Needed in some multivariate analyses.
}
\usage{
dummy(f, simplify = TRUE)
}
\arguments{
  \item{f}{
vector (internally transformed into factor).
}
  \item{simplify}{
logical indicating if the last column of the response matrix should be removed (to avoid model overfitting).
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
fac <- gl(3,5,labels=LETTERS[1:3])
dummy(fac)
}