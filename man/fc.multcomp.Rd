\name{fc.multcomp}
\alias{fc.multcomp}
\title{
Pairwise comparisons for the interaction between a factor and a covariate
}
\description{
Perform pairwise comparisons between groups, based on a model, for one factor or the interaction between a factor and a covariate. The function is based on \link[multcomp]{glht}, so deals with any model accepted by this function.
}
\usage{
fc.multcomp(model, term = NULL, mat = NULL, p.method = "fdr")
}
\arguments{
  \item{model}{
any model accepted by \link[multcomp]{glht}.
}
  \item{term}{
character string giving the term on which comparisons are based (a factor or the interaction between a factor and a covariate).
}
  \item{mat}{
matrix of contrasts (see 'Details'), either a matrix or a data frame.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
In matrices of contrasts, each line is a comparison (= contrast) and each colum is a level of the factor. Rules for writing contrasts are:

- levels not involved in the comparison must have a null value

- levels to be compared must have opposite signs

- levels can be grouped (for example 2 -1 -1 give a comparison of the first level against the group composed by the two others)

- the sum of all values of a contrast must be null.
}
\value{
  \item{method}{a character string giving the name of the test computed.}
  \item{model}{model call.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[multcomp]{glht}}
}
\examples{
# 'kidney' dataset of package 'survival'
require(survival)
require(multcomp)

model <- coxph(Surv(time,status)~age*disease+factor(sex),data=kidney)

# Default: all pairwise comparisons
fc.multcomp(model,"disease")
fc.multcomp(model,"age:disease")

# Dunnett contrasts
n <- n <- 1:nlevels(kidney$disease)
names(n) <- levels(kidney$disease)
mat <- contrMat(n)
fc.multcomp(model,"disease",mat)
}