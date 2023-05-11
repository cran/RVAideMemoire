\name{spearman.ci}
\alias{spearman.ci}
\title{
Confidence interval of a Spearman's rank correlation coefficient
}
\description{
Computes the confidence interval of a Spearman's rank correlation coefficient by bootstraping.
}
\usage{
spearman.ci(var1, var2, nrep = 1000, conf.level = 0.95)
}
\arguments{
  \item{var1}{
numeric vector (first variable).
}
  \item{var2}{
nuermic verctor (second variable).
}
  \item{nrep}{
number of replicates for bootstraping.
}
  \item{conf.level}{
confidence level of the interval.
}
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{conf.level}{confidence level.}
  \item{rep}{number of replicates.}
  \item{estimate}{Spearman's rank correlation coefficient.}
  \item{conf.int}{confidence interval.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{cor.test}}, \code{\link[boot]{boot}}
}
\examples{
var1 <- sample(1:50,15,replace=TRUE)
var2 <- sample(1:50,15,replace=TRUE)
spearman.ci(var1,var2)
}