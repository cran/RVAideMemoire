\name{G.multcomp}
\alias{G.multcomp}
\title{
Pairwise comparisons after a G-test
}
\description{
Performs pairwise comparisons after a global G-test.
}
\usage{
G.multcomp(x, p.method = "fdr")
}
\arguments{
  \item{x}{
numeric vector (counts).
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
Since a G-test is an approximate test, an exact test is preferable when the number of individuals is small (200 is a reasonable minimum). See \code{\link[RVAideMemoire]{multinomial.multcomp}} in that case.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{G.test}}, \code{\link[RVAideMemoire]{multinomial.test}}, \code{\link[RVAideMemoire]{multinomial.multcomp}}
}
\examples{
counts <- c(49,30,63,59)
G.test(counts)
G.multcomp(counts)
}