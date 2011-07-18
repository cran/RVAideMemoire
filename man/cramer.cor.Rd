\name{cramer.cor}
\alias{cramer.cor}
\title{
Cramer's association coefficient
}
\description{
Compute the Cramer's association coefficient between 2 nomial variables, and its confidence interval (by bootstraping).
}
\usage{
cramer.cor(var1, var2, rep = 1000, conf.level = 0.95)
}
\arguments{
  \item{var1}{
factor (first variable).
}
  \item{var2}{
factor (second variable).
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
  \item{V}{Cramer's coefficient.}
  \item{interval}{confidence interval.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[boot]{boot}}
}
\examples{
var1<-sample(LETTERS[1:3],15,replace=TRUE)
var2<-sample(letters[1:3],15,replace=TRUE)
cramer.cor(var1,var2)
}