\name{adonis.II}
\alias{adonis.II}
\title{
Type II permutation MANOVA using distance matrices
}
\description{
This function is a wrapper to \code{\link[vegan]{adonis2}} but performs type II tests (whereas \code{\link[vegan]{adonis2}} performs type I).
}
\usage{
adonis.II(formula, data, ...)
}
\arguments{
  \item{formula}{
a typical model formula such as \code{Y~A+B*C}, but where \code{Y} is either a dissimilarity object (inheriting from class \code{"dist"}) or data frame or a matrix; \code{A}, \code{B}, and \code{C} may be factors or continuous variables.
}
  \item{data}{
the data frame from which \code{A}, \code{B} and \code{C} would be drawn.
}
  \item{\dots}{
additional arguments to \code{\link[vegan]{adonis2}}. See help of this function.
}
}
\details{
See \code{\link[vegan]{adonis2}} for detailed explanation of what is done. The only difference with \code{\link[vegan]{adonis2}} is that \code{adonis.II} performs type II tests instead of type I.
}
\value{
  a data frame of class \code{"anova"}.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
require(vegan)
data(dune)
data(dune.env)

# Compare:
adonis2(dune~Management*A1,data=dune.env,permutations=99)
adonis2(dune~A1*Management,data=dune.env,permutations=99)

# With:
adonis.II(dune~Management*A1,data=dune.env,permutations=99)
adonis.II(dune~A1*Management,data=dune.env,permutations=99)
}