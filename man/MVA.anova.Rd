\name{MVA.anova}
\alias{MVA.anova}
\title{
Type II permutation test for constrained multivariate analyses
}
\description{
This function is a wrapper to \code{anova.cca(...,by="terms")} but performs type II tests (whereas \code{\link[vegan]{anova.cca}} performs type I).
}
\usage{
MVA.anova(object, ...)
}
\arguments{
  \item{object}{
  a result object from \code{\link[vegan]{cca}}, \code{\link[vegan]{rda}}, \code{\link[vegan]{capscale}} or \code{\link[vegan]{dbrda}}.
}
  \item{\dots}{
  additional arguments to \code{\link[vegan]{anova.cca}} (can be \code{permutations}, \code{model}, \code{parallel} and/or \code{strata}). See help of this function.
}
}
\details{
See \code{\link[vegan]{anova.cca}} for detailed explanation of what is done. The only difference with \code{\link[vegan]{anova.cca}} is that \code{MVA.anova} performs type II tests instead of type I.

See example of \code{\link[RVAideMemoire]{adonis.II}} for the difference between type I (sequential) and type II tests.
}
\value{
  a data frame of class \code{"anova"}.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}