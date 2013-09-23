\name{mod}
\alias{mod}
\title{
Mode
}
\description{
Compute the mode of a vector. The function makes the difference between continuous and discontinuous variables (which are made up of integers only).
}
\usage{
mod(x)
}
\arguments{
  \item{x}{
numeric vector.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{density}}
}
\examples{
# Continuous variable
x <- rnorm(100)
mod(x)

# Discontinuous variable
y <- rpois(100,2)
mod(y)
}
