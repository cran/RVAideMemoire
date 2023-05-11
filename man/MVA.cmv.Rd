\name{MVA.cmv}
\alias{MVA.cmv}
\title{
Cross model validation
}
\description{
Performs cross model validation (2CV) with different PLS analyses.
}
\usage{
MVA.cmv(X, Y, repet = 10, kout = 7, kinn = 6, ncomp = 8, scale = TRUE,
  model = c("PLSR", "CPPLS", "PLS-DA", "PPLS-DA", "PLS-DA/LDA", "PLS-DA/QDA",
  "PPLS-DA/LDA", "PPLS-DA/QDA"), crit.inn = c("RMSEP", "Q2", "NMC"),
  Q2diff = 0.05, lower = 0.5, upper = 0.5, Y.add = NULL, weights = rep(1, nrow(X)),
  set.prior = FALSE, crit.DA = c("plug-in", "predictive", "debiased"), ...)
}
\arguments{
  \item{X}{
a data frame of independent variables.
}
  \item{Y}{
the dependent variable(s): numeric vector, data frame of quantitative variables or factor.
}
  \item{repet}{
an integer giving the number of times the whole 2CV procedure has to be repeated.
}
  \item{kout}{
an integer giving the number of folds in the outer loop (can be re-set internally if needed).
}
  \item{kinn}{
an integer giving the number of folds in the inner loop (can be re-set internally if needed). Cannot be \code{> kout}.
}
  \item{ncomp}{
an integer giving the maximal number of components to be tested in the inner loop (can be re-set depending on the size of the train sets).
}
  \item{scale}{
logical indicating if data should be scaled (see Details).
}
  \item{model}{
the model to be fitted (see Details).
}
  \item{crit.inn}{
the criterion to be used to choose the number of components in the inner loop. Root Mean Square Error of Prediction (\code{"RMSEP"}, default) and Q2 (\code{"Q2"}) are only used for PLSR and CPPLS, whereas the Number of MisClassifications (\code{"NMC"}) is only used for discriminant analyses.
}
  \item{Q2diff}{
the threshold to be used if the number of components is chosen according to Q2. The next component is added only if it makes the Q2 increase more than \code{Q2diff} (5\% by default).
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
only used when a second analysis (LDA or QDA) is performed. If \code{TRUE}, the prior probabilities of class membership are defined according to the mean weight of individuals belonging to each class. If \code{FALSE}, prior probabilities are obtained from the data sets on which LDA/QDA models are built.
}
  \item{crit.DA}{
criterion used to predict class membership when a second analysis (LDA or QDA) is used. See \code{\link[MASS]{predict.lda}}.
}
  \item{...}{
other arguments to pass to \code{\link[pls:mvr]{plsr}} (PLSR, PLS-DA) or \code{\link[pls:mvr]{cppls}} (CPPLS, PPLS-DA).
}
}
\details{
Cross model validation is detailed is Szymanska et al (2012). Some more details about how this function works:

- when a discriminant analysis is used (\code{"PLS-DA"}, \code{"PPLS-DA"}, \code{"PLS-DA/LDA"}, \code{"PLS-DA/QDA"}, \code{"PPLS-DA/LDA"} or \code{"PPLS-DA/QDA"}), the training sets (test set itself in the inner loop, test+validation sets in the outer loop) are generated in respect to the relative proportions of the levels of \code{Y} in the original data set (see \code{\link[RVAideMemoire]{splitf}}).

- \code{"PLS-DA"} is considered as PLS2 on a dummy-coded response. For a PLS-DA based on the CPPLS algorithm, use \code{"PPLS-DA"} with \code{lower} and \code{upper} limits of the power parameters set to \code{0.5}.

- if a second analysis is used (\code{"PLS-DA/LDA"}, \code{"PLS-DA/QDA"}, \code{"PPLS-DA/LDA"} or \code{"PPLS-DA/QDA"}), a LDA or QDA is built on scores of the first analysis (PLS-DA or PPLS-DA) also in the inner loop. The classification error rate, based on this second analysis, is used to choose the number of components.


If \code{scale = TRUE}, the scaling is done as this:

- for each step of the outer loop (i.e. \code{kout} steps), the rest set is pre-processed by centering and unit-variance scaling. Means and standard deviations of variables in the rest set are then used to scale the test set.

- for each step of the inner loop (i.e. \code{kinn} steps), the training set is pre-processed by centering and unit-variance scaling. Means and standard deviations of variables in the training set are then used to scale the validation set.
}
\value{
  \item{model}{model used.}
  \item{type}{type of model used.}
  \item{repet}{number of times the whole 2CV procedure was repeated.}
  \item{kout}{number of folds in the outer loop.}
  \item{kinn}{number of folds in the inner loop.}
  \item{crit.inn}{criterion used to choose the number of components in the inner loop.}
  \item{crit.DA}{criterion used to classify individuals of the test and validation sets.}
  \item{Q2diff}{threshold used if the number of components is chosen according to Q2.}
  \item{groups}{levels of \code{Y} if it is a factor.}
  \item{models.list}{list of of models generated (\code{repet*kout} models), for PLSR, CPPLS, PLS-DA and PPLS-DA.}
  \item{models1.list}{list of of (P)PLS-DA models generated (\code{repet*kout} models), for PLS-DA/LDA, PLS-DA/QDA, PPLS-DA/LDA and PPLS-DA/QDA.}
  \item{models2.list}{list of of LDA/QDA models generated (\code{repet*kout} models), for PLS-DA/LDA, PLS-DA/QDA, PPLS-DA/LDA and PPLS-DA/QDA.}
  \item{RMSEP}{RMSEP computed from the models used in the outer loops (\code{repet} values).}
  \item{Q2}{Q2 computed from the models used in the outer loops (\code{repet} values).}
  \item{NMC}{Classification error rate computed from the models used in the outer loops (\code{repet} values).}
  \item{confusion}{Confusion matrices computed from the models used in the outer loops (\code{repet} values).}
  \item{pred.prob}{Probability of each individual of being of each level of \code{Y}.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\references{
Szymanska E, Saccenti E, Smilde AK and Westerhuis J (2012) Double-check: validation of diagnostic statistics for PLS-DA models in metabolomics studies. Metabolomics (2012) 8:S3-S16.
}
\seealso{
\code{\link[RVAideMemoire:predict.MVA.cv]{predict.MVA.cmv}}, \code{\link[pls]{mvr}}, \code{\link[MASS]{lda}}, \code{\link[MASS]{qda}}
}
\examples{
require(pls)
require(MASS)

# PLSR
data(yarn)
\dontrun{MVA.cmv(yarn$NIR,yarn$density,model="PLSR")}

# PPLS-DA coupled to LDA
data(mayonnaise)
\dontrun{MVA.cmv(mayonnaise$NIR,factor(mayonnaise$oil.type),model="PPLS-DA/LDA",crit.inn="NMC")}
}
