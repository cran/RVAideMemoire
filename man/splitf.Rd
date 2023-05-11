\name{splitf}
\alias{splitf}
\title{
Divide into groups respecting relative proportions
}
\description{
Divides a data frame randomly, but respecting the relative proportions of levels of a factor in the original data frame. Each subset has roughly the same number of individuals, and the same relative proportions in respect to levels of the given factor.
}
\usage{
splitf(set, fac, k)
}
\arguments{
  \item{set}{
a data frame containing values to be divided into groups.
}
  \item{fac}{
a reference factor giving the relative proportions to be respected in each subset of \code{set}.
}
  \item{k}{
an integer giving the number of subsets to be generated.
}
}
\value{
A list of subsets of \code{set}.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{split}}
}
\examples{
data(iris)
iris2 <- iris[c(1:50,51:80,101:120),]

# Proportions to be respected
table(iris2$Species)/nrow(iris2)

# Splitting
result <- splitf(iris2,iris2$Species,3)

# All subsets have the same size
lapply(result,nrow)

# And respect the initial proportions
lapply(result,function(x) table(x$Species)/nrow(x))
}