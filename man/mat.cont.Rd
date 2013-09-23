\name{mat.cont}
\alias{mat.cont}
\title{
Matrix of contrasts generation
}
\description{
Return a matrix of contrasts for pairwise comparisons. By default all pairwise comparisons are returned.
}
\usage{
mat.cont(lev, ref = NULL, restrict = NULL, exclude = FALSE)
}
\arguments{
  \item{lev}{a vector of character strings giving levels to be compared. This includes levels of a single factor or levels of an interaction (between factors or factors and covariates).
}
  \item{ref}{
optional reference level (only contrasts involving this level are generated).
}
  \item{restrict}{
optional vector of levels (only contrasts involving these levels are generated).
}
  \item{exclude}{
logical. If \code{TRUE} and if \code{restrict} is not \code{NULL}, levels which are not involved in comparisons are not present in the matrix (to be used when the model has been generated with the \code{subset} option).
}
}
\details{
In these matrices, each line is a comparison (= contrast) and each colum is a level of the factor. Rules for writing contrasts are:

- levels not involved in the comparison must have a null value

- levels to be compared must have opposite signs

- levels can be grouped (for example 2 -1 -1 give a comparison of the first level against the group composed by the two others)

- the sum of all values of a contrast must be null.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[lsmeans]{lsmeans}}, \code{\link[RVAideMemoire]{user.cont}}
}
\examples{
# A factor with 5 levels
fact <- gl(5,10,labels=LETTERS[1:5])

# A matrix of contrasts with all possible pairwise comparisons
mat.cont(levels(fact))

# A matrix with only comparisons to level "A"
mat.cont(levels(fact),ref="A")

# A matrix with all pairwise comparisons between "A", "B" and "E"
mat.cont(levels(fact),restrict=c("A","B","E"))

# The same with levels "C" and "D" excluded
mat.cont(levels(fact),restrict=c("A","B","E"),exclude=TRUE)


# With an interaction between two factors
fact2 <- gl(2,25,labels=letters[1:2])
mat.cont(levels(fact:fact2))


# With an interaction between a factor and a covariate
covar <- runif(50,10,20)
lev <- paste("covar:fact",levels(fact),sep="")
mat.cont(lev)
}
