\name{MVA.test}
\alias{MVA.test}
\title{
Significance test based on cross (model) validation
}
\description{
Performs a permutational significance test based on cross (model) validation with different PLS and/or discriminant analyses. See \code{\link[RVAideMemoire]{MVA.cv}} and \code{\link[RVAideMemoire]{MVA.cmv}} for more details about how cross (model) validation is performed.
}
\usage{
MVA.test(X, Y, cmv = FALSE, ncomp = 8, kout = 7, kinn = 6, scale = TRUE,
  model = c("PLSR", "CPPLS", "PLS-DA", "PPLS-DA", "LDA", "QDA", "PLS-DA/LDA",
  "PLS-DA/QDA", "PPLS-DA/LDA","PPLS-DA/QDA"), Q2diff = 0.05, lower = 0.5,
  upper = 0.5, Y.add = NULL, weights = rep(1, nrow(X)), set.prior = FALSE,
  crit.DA = c("plug-in", "predictive", "debiased"), p.method = "fdr",
  nperm = 999, progress = TRUE, ...)
}
\arguments{
  \item{X}{
a data frame of independent variables.
}
  \item{Y}{
the dependent variable(s): numeric vector, data frame of quantitative variables or factor.
}
  \item{cmv}{
a logical indicating if the values (Q2 or NMC) should be generated through cross-validation (classical K-fold process) or cross model validation (inner + outer loops).
}
  \item{ncomp}{
an integer giving the number of components to be used to generate all submodels (cross-validation) or the maximal number of components to be tested in the inner loop (cross model validation). Can be re-set internally if needed. Does not concern LDA and QDA.
}
  \item{kout}{
an integer giving the number of folds (cross-validation) or the number of folds in the outer loop (cross-model validation).  Can be re-set internally if needed.
}
  \item{kinn}{
an integer giving the number of folds in the inner loop (cross model validation only).  Can be re-set internally if needed. Cannot be \code{> kout}.
}
  \item{scale}{
logical indicating if data should be scaled. See help of \code{\link[RVAideMemoire]{MVA.cv}} and \code{\link[RVAideMemoire]{MVA.cmv}}.
}
  \item{model}{
the model to be fitted.
}
  \item{Q2diff}{
the threshold to be used if the number of components is chosen according to Q2 (cross model validation only).
}
  \item{lower}{
a vector of lower limits for power optimisation in CPPLS or PPLS-DA (see \code{\link[pls]{cppls.fit}}).
}
  \item{upper}{
a vector of upper limits for power optimisation in CPPLS or PPLS-DA (see \code{\link[pls]{cppls.fit}}).
}
  \item{Y.add}{
a vector or matrix of additional responses containing relevant information about the observations, in CPPLS or PPLS-DA (see \code{\link[pls]{cppls.fit}}).
}
  \item{weights}{
a vector of individual weights for the observations, in CPPLS or PPLS-DA (see \code{\link[pls]{cppls.fit}}).
}
  \item{set.prior}{
only used when a LDA or QDA is performed (coupled or not with a PLS model). If \code{TRUE}, the prior probabilities of class membership are defined according to the mean weight of individuals belonging to each class. If \code{FALSE}, prior probabilities are obtained from the data sets on which LDA/QDA models are built.
}
  \item{crit.DA}{
criterion used to predict class membership when a LDA or QDA is used. See \code{\link[MASS]{predict.lda}}.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
  \item{...}{
other arguments to pass to \code{\link[pls]{plsr}} (PLSR, PLS-DA) or \code{\link[pls]{cppls}} (CPPLS, PPLS-DA).
}
}
\details{
When \code{Y} consists in quantitative response(s), the null hypothesis is that each response is not predicted better than what would happen by chance. In this case, Q2 is used as the test statistic. When \code{Y} contains several responses, a p-value is computed for each response and p-values are corrected for multiple testing.

When \code{Y} is a factor, the null hypothesis is that the factor has no discriminant ability. In this case, the classification error rate (NMC) is used as the test statistic.

Whatever the response, the reference value of the test statistics is obtained by averaging 20 values coming from independently performed cross (model) validation on the original data.

The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data, plus additional information.}
  \item{statistic}{the value of the test statistics.}
  \item{permutations}{the number of permutations.}
  \item{p.value}{the p-value of the test.}
  \item{p.adjust.method}{a character string giving the method for p-values correction.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\references{
Westerhuis J, Hoefsloot HCJ, Smit S, Vis DJ, Smilde AK, van Velzen EJJ, van Duijnhoven JPM and van Dorsten FA (2008) Assessment of PLSDA cross validation. Metabolomics 4:81-89.
}
\seealso{
\code{\link[RVAideMemoire]{MVA.cv}}, \code{\link[RVAideMemoire]{MVA.cmv}}
}
\examples{
require(pls)
require(MASS)

# PLSR
data(yarn)
\dontrun{MVA.test(yarn$NIR,yarn$density,cmv=TRUE,model="PLSR")}

# PPLS-DA coupled to LDA
data(mayonnaise)
\dontrun{MVA.test(mayonnaise$NIR,factor(mayonnaise$oil.type),model="PPLS-DA/LDA")}
}
