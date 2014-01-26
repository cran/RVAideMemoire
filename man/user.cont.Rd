\name{user.cont}
\alias{user.cont}
\title{
User defined contrasts for LSMeans
}
\description{
Return a function usable by \code{\link[lsmeans]{lsmeans}} for user defined contrasts.
}
\usage{
user.cont(cont)
}
\arguments{
  \item{cont}{any matrix of contrasts (see 'Details').
}
}
\value{
  \item{user.cont.lsmc}{the function to be called by \code{\link[lsmeans]{lsmeans}}}
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
\code{\link[lsmeans]{lsmeans}}
}
\examples{
require(car)
require(lsmeans)

response <- c(rpois(30,1),rpois(30,3),rpois(30,10)) 
fact <- gl(3,30,labels=LETTERS[1:3])
model <- glm(response~fact,family="poisson")
Anova(model)
mat <- matrix(c(1,-1,0,0,1,-1,2,-1,-1),nrow=3,byrow=TRUE,dimnames=list(levels(fact),1:3))
mat
cont.lsmc <- user.cont(mat)
lsmeans(model,cont~fact)
}
