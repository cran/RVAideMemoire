\name{cv}
\alias{cv}
\title{
Coefficient of variation
}
\description{
Computes the coefficient of variation of a vector.
}
\usage{
cv(x, abs = TRUE, pc = TRUE)
}
\arguments{
  \item{x}{
numeric vector.
}
  \item{abs}{
logical. If \code{TRUE} the coefficient is expressed in absolute value.
}
  \item{pc}{
logical. If \code{TRUE} the coefficient is expressed in percentage.
}
}
\details{
The function deals with missing values.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
cv(rnorm(30))
}