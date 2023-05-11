\name{mod}
\alias{mod}
\title{
Mode
}
\description{
Computes the mode of a vector. The function makes the difference between continuous and discontinuous variables (which are made up of integers only). By extention, it also gives the most frequent value in a character vector or a factor.
}
\usage{
mod(x)
}
\arguments{
  \item{x}{
vector (numeric, character or factor).
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
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

# Character vector
z <- sample(LETTERS[1:3],20,replace=TRUE)
mod(z)
}
