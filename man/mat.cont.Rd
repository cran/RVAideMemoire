\name{mat.cont}
\alias{mat.cont}
\title{
Matrix of contrasts generation
}
\description{
Return a matrix for pairwise comparisons with the contrasts method.
}
\usage{
mat.cont(f, ref = NULL, restrict = NULL, exclude = FALSE)
}
\arguments{
  \item{f}{factor giving the groups to be compared.
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
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[doBy]{esticon}}, \code{\link[RVAideMemoire]{adjust.esticon}}
}
\examples{
# A factor with 5 levels
fact <- gl(5,10,labels=LETTERS[1:5])

# A matrix of contrasts with all possible pairwise comparisons
mat.cont(fact)

# A matrix with only comparisons to level "A"
mat.cont(fact,ref="A")

# A matrix with all pairwise comparisons between "A", "B" and "E"
mat.cont(fact,restrict=c("A","B","E"))

# The same with levels "C" and "D" excluded
mat.cont(fact,restrict=c("A","B","E"),exclude=TRUE)
}
