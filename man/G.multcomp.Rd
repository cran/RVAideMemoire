\name{G.multcomp}
\alias{G.multcomp}
\title{
Pairwise comparisons after a G-test
}
\description{
Perform pairwise comparisons after a global G-test.
}
\usage{
G.multcomp(x, p.method = "fdr")
}
\arguments{
  \item{x}{
numeric vector (counts).
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{G.test}}
}
\examples{
counts <- c(5,15,23,8,14)
G.test(counts)
G.multcomp(counts)
}