\name{seq2}
\alias{seq2}
\title{
Sequence generation
}
\description{
Generates a regular sequence from the minimum to the maximum of a vector.
}
\usage{
seq2(x, int = 999)
}

\arguments{
  \item{x}{
numeric vector.
}
  \item{int}{
number of values to be generated (\code{int} breaks).
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{seq}}
}
\examples{
seq2(rnorm(30))
}