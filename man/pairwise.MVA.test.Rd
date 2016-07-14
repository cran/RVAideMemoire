\name{pairwise.MVA.test}
\alias{pairwise.MVA.test}
\title{
Pairwise permutational tests based on cross (model) validation
}
\description{
Performs pairwise comparisons between group levels with corrections for multiple testing, using \code{\link[RVAideMemoire]{MVA.test}}.
}
\usage{
pairwise.MVA.test(X, fact, p.method = "fdr", cmv = FALSE, ncomp = 8,
  kout = 7, kinn = 6, model = c("PLS-DA", "PPLS-DA", "LDA", "QDA",
  "PLS-DA/LDA", "PLS-DA/QDA", "PPLS-DA/LDA","PPLS-DA/QDA"),
  nperm = 999, progress = TRUE, ...)
}
\arguments{
  \item{X}{
a data frame of independent variables.
}
  \item{fact}{
grouping factor.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{cmv}{
a logical indicating if the test statistic (NMC) should be generated through cross-validation (classical K-fold process) or cross model validation (inner + outer loops).
}
  \item{ncomp}{
an integer giving the number of components to be used to generate all submodels (cross-validation) or the maximal number of components to be tested in the inner loop (cross model validation).   Can be re-set internally if needed. Does not concern LDA and QDA.
}
  \item{kout}{
an integer giving the number of folds (cross-validation) or the number of folds in the outer loop (cross-model validation).  Can be re-set internally if needed.
}
  \item{kinn}{
an integer giving the number of folds in the inner loop (cross model validation only).  Can be re-set internally if needed. Cannot be \code{> kout}.
}
  \item{model}{
the model to be fitted.
}
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
  \item{...}{
other arguments to pass to \code{\link[RVAideMemoire]{MVA.test}}.
}
}
\details{
The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{method}{a character string indicating what type of tests were performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{permutations}{number of permutations.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{MVA.test}}
}
\examples{
require(pls)
data(mayonnaise)

# PPLS-DA
\dontrun{pairwise.MVA.test(mayonnaise$NIR,factor(mayonnaise$oil.type),model="PPLS-DA")}

# The function needs a long calculation time!
}