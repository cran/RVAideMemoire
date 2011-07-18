\name{spearman.ci}
\alias{spearman.ci}
\title{
Confidence interval of a Spearman's rank correlation coefficient
}
\description{
Compute the confidence interval of a Spearman's rank correlation coefficient by bootstraping.
}
\usage{
spearman.ci(var1, var2, rep = 1000, conf.level = 0.95)
}
\arguments{
  \item{var1}{
numeric vector (first variable).
}
  \item{var2}{
nuermic verctor (second variable).
}
  \item{rep}{
number of replicates for bootstrap.
}
  \item{conf.level}{
confidence level.
}
}
\value{
  \item{conf.level}{confidence level.}
  \item{rep}{number of replicates.}
  \item{coeff}{Spearman's rank correlation coefficient.}
  \item{interval}{confidence interval.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{cor.test}}, \code{\link[boot]{boot}}
}
\examples{
var1<-sample(1:50,15,replace=TRUE)
var2<-sample(1:50,15,replace=TRUE)
spearman.ci(var1,var2)
}