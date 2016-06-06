\name{MVA.cv}
\alias{MVA.cv}
\title{
Cross validation
}
\description{
Performs cross validation with different PLS and/or discriminant analyses.
}
\usage{
MVA.cv(X, Y, repet = 10, k = 7, ncomp = 8, model = c("PLSR","CPPLS",
  "PLS-DA", "PPLS-DA", "LDA", "QDA", "PLS-DA/LDA", "PLS-DA/QDA", "PPLS-DA/LDA",
  "PPLS-DA/QDA"), lower = 0.5, upper = 0.5, Y.add = NULL, weights = rep(1,
  nrow(X)), set.prior = FALSE, crit.DA = c("plug-in", "predictive",
  "debiased"), ...)
}
\arguments{
  \item{X}{
a data frame of independent variables.
}
  \item{Y}{
the dependent variable(s): numeric vector, data frame of quantitative variables or factor.
}
  \item{repet}{
an integer giving the number of times the whole procedure has to be repeated.
}
  \item{k}{
an integer giving the number of folds (can be re-set internally if needed).
}
  \item{ncomp}{
an integer giving the number of components to be used for all models except LDA and QDA (can be re-set depending on the size of the train sets).
}
  \item{model}{
the model to be fitted (see Details).
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
  \item{...}{
other arguments to pass to \code{\link[pls]{plsr}} (PLSR, PLS-DA) or \code{\link[pls]{cppls}} (CPPLS, PPLS-DA).
}
}
\details{
When a discriminant analysis is used (\code{"PLS-DA"}, \code{"PPLS-DA"}, \code{"LDA"}, \code{"QDA"}, \code{"PLS-DA/LDA"}, \code{"PLS-DA/QDA"}, \code{"PPLS-DA/LDA"} or \code{"PPLS-DA/QDA"}), the training sets are generated in respect to the relative proportions of the levels of \code{Y} in the original data set (see \code{\link[RVAideMemoire]{splitf}}).

\code{"PLS-DA"} is considered as PLS2 on a dummy-coded response. For a PLS-DA based on the CPPLS algorithm, use \code{"PPLS-DA"} with \code{lower} and \code{upper} limits of the power parameters set to \code{0.5}.
}
\value{
  \item{model}{model used.}
  \item{type}{type of model used.}
  \item{repet}{number of times the whole 2CV procedure was repeated.}
  \item{k}{number of folds.}
  \item{k}{number of components used.}
  \item{crit.DA}{criterion used to classify individuals of the test sets.}
  \item{models.list}{list of of models generated (\code{repet*k} models), for PLSR, CPPLS, PLS-DA, PPLS-DA, LDA and QDA.}
  \item{models1.list}{list of of (P)PLS-DA models generated (\code{repet*k} models), for PLS-DA/LDA, PLS-DA/QDA, PPLS-DA/LDA and PPLS-DA/QDA.}
  \item{models2.list}{list of of LDA/QDA models generated (\code{repet*k} models), for PLS-DA/LDA, PLS-DA/QDA, PPLS-DA/LDA and PPLS-DA/QDA.}
  \item{RMSEP}{RMSEP vales (\code{repet} values).}
  \item{Q2}{Q2 values (\code{repet} values).}
  \item{NMC}{NMC values (\code{repet} values).}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{predict.MVA.cmv}}, \code{\link[pls]{mvr}}, \code{\link[MASS]{lda}}, \code{\link[MASS]{qda}}
}
\examples{
require(pls)
require(MASS)

# PLSR
data(yarn)
\dontrun{MVA.cv(yarn$NIR,yarn$density,model="PLSR")}

# PPLS-DA coupled to LDA
data(mayonnaise)
\dontrun{MVA.cv(mayonnaise$NIR,factor(mayonnaise$oil.type),model="PPLS-DA/LDA")}
}
