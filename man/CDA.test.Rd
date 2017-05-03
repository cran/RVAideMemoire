\name{CDA.test}
\alias{CDA.test}
\title{
Significance test for CDA
}
\description{
Performs a significance test for correspondence discriminant analysis. See Details.
}
\usage{
CDA.test(X, fact, ncomp = NULL, ...)
}
\arguments{
  \item{X}{
a data frame of dependent variables (typically contingency or presence-absence table).
}
  \item{fact}{
factor giving the groups.
}
  \item{ncomp}{
an integer giving the number of components to be used for the test. If \code{NULL} \code{nlevels(fact)-1} are used. See Details.
}
  \item{...}{
other arguments to pass to \code{\link{summary.manova}}. See Details.
}
}
\details{
CDA consists in two steps: building a correspondence analysis (CA) on \code{X}, then using row coordinates on all CA components as input variables for a linear discriminant analysis. \code{MVA.test} builds the intermediate CA, then uses the first \code{ncomp} components to test for an effect of \code{fact}. If 1 component is used the test is an ANOVA, if more than 1 component are used the test is a MANOVA.
}
\value{
An ANOVA or MANOVA table.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[ade4]{discrimin.coa}}, \code{\link{summary.manova}}
}
\examples{
require(ade4)
data(perthi02)

CDA.test(perthi02$tab,perthi02$cla)
}
