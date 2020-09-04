\name{pairwise.CDA.test}
\alias{pairwise.CDA.test}
\title{
Pairwise comparisons for CDA
}
\description{
Performs pairwise comparisons between group levels with corrections for multiple testing, using \code{\link[RVAideMemoire]{CDA.test}}.
}
\usage{
pairwise.CDA.test(X, fact, ncomp = NULL, p.method = "fdr", ...)
}
\arguments{
  \item{X}{
a data frame of dependent variables (typically contingency or presence-absence table).
}
  \item{fact}{
factor giving the groups.
}
  \item{ncomp}{
an integer giving the number of components to be used for the test. If \code{NULL} \code{nlevels(fact)-1} are used.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{...}{
other arguments to pass to \code{\link[RVAideMemoire]{CDA.test}}.
}
}
\details{
See \code{\link[RVAideMemoire]{CDA.test}}.
}
\value{
  \item{method}{a character string indicating what type of tests were performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{CDA.test}}
}
\examples{
require(ade4)
data(perthi02)

CDA.test(perthi02$tab,perthi02$cla)
pairwise.CDA.test(perthi02$tab,perthi02$cla)
}