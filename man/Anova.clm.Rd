\name{Anova.clm}
\alias{Anova.clm}
\alias{Anova.clmm}
\title{
Anova Tables for Cumulative Link (Mixed) Models
}
\description{
These functions are methods for \code{\link[car]{Anova}} to calculate type-II or type-III analysis-of-deviance tables for model objects produced by \code{\link[ordinal]{clm}} and \code{\link[ordinal]{clmm}}. Likelihood-ratio tests are calculated in both cases.
}
\usage{
\method{Anova}{clm}(mod, type = c("II", "III", 2, 3), ...)

\method{Anova}{clmm}(mod, type = c("II", "III", 2, 3), ...)
}
\arguments{
  \item{mod}{
\code{clm} or \code{clmm} object.
}
  \item{type}{
type of test, \code{"II"}, \code{"III"}, \code{2} or \code{3}.
}

  \item{\dots}{
additional arguments to \code{\link[car]{Anova}}. Not usable here.
}
}
\details{
See help of the \code{\link[car]{Anova}} for a detailed explanation of what "type II" and "typ III" mean.
}
\value{
See \code{\link[car]{Anova}}.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[car]{Anova}}, \code{\link[ordinal]{clm}}, \code{\link[ordinal]{clmm}}
}
