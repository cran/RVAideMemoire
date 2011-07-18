\name{se}
\alias{se}
\title{
Standard error of the mean
}
\description{
Compute the standard error of the mean of a vector.
}
\usage{
se(x)
}
\arguments{
  \item{x}{
numeric vector.
}
}
\details{
The function deals with missing values.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
se(rnorm(30))
}