\name{spearman.cor.multcomp}
\alias{spearman.cor.multcomp}
\title{
Comparison of several Spearman's rank correlation coefficients
}
\description{
Computes Bonferroni-adjusted confidence intervals of a series of Spearman's rank correlation coefficients, for multiple comparisons. Confidence intervals are computed by bootstraping.
}
\usage{
spearman.cor.multcomp(var1, var2, fact, alpha = 0.05, nrep = 1000)
}
\arguments{
  \item{var1}{
numeric vector (first variable).
}
  \item{var2}{
numeric vector (second variable).
}
  \item{fact}{
factor (groups).
}
  \item{alpha}{
significance level.
}
  \item{nrep}{
number of replicates for bootstraping.
}
}
\details{
Confidence intervals which do not overlap indicate correlation coefficients significantly different at \code{alpha}.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{tab}{data frame of correlation coefficients with confidence intervals}
  \item{alpha}{significance level.}
  \item{nrep}{number of replicates for bootstraping.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{spearman.ci}}
}
\examples{
set.seed(1510)
var1 <- c(1:15+rnorm(15,0,2),1:15+rnorm(15,0,2),1:15+rnorm(15,0,2))
var2 <- c(-1:-15+rnorm(15,0,2),1:15+rnorm(15,0,2),1:15+rnorm(15,0,2))
fact <- gl(3,15,labels=LETTERS[1:3])
spearman.cor.multcomp(var1,var2,fact)
# B and C similar but different from A
}