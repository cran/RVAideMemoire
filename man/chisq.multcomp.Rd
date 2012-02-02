\name{chisq.multcomp}
\alias{chisq.multcomp}
\title{
Pairwise comparisons after a chi-squared goodness-of-fit test
}
\description{
Perform pairwise comparisons after a global chi-squared goodness-of-fit test.
}
\usage{
chisq.multcomp(x, p.method = "fdr")
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
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{chisq.test}}
}
\examples{
counts <- c(5,15,23,8,14)
chisq.test(counts)
chisq.multcomp(counts)
}