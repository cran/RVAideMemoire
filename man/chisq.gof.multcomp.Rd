\name{chisq.gof.multcomp}
\alias{chisq.gof.multcomp}
\title{
Pairwise comparisons after chi2 goodness-of-fit test
}
\description{
Perform pairwise comparisons after a global chi2 goodness-of-fit test.
}
\usage{
chisq.gof.multcomp(x, p = rep(1/length(x), length(x)), p.method = "fdr")
}
\arguments{
  \item{x}{
nuermic vector (counts).
}
  \item{p}{
theoretical proportions.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{observed}{observed counts.}
  \item{expected}{expected counts.}
  \item{p.method}{method for p-values correction.}
  \item{chi2}{statistics of each test.}
  \item{p}{corrected p-values.}
  \item{com}{data.frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{chisq.test}}
}
\examples{
counts<-c(5,15,23,8,14)
chisq.test(counts)
chisq.gof.multcomp(counts)

p.theo<-c(0.1,0.4,0.3,0.15,0.05)
chisq.test(counts,p=p.theo)
chisq.gof.multcomp(counts,p=p.theo)
}