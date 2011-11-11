\name{cor.conf}
\alias{cor.conf}
\title{
Equality of a Pearson's linear correlation coefficient to a given value
}
\description{
Perform a test for equality of a Pearson's linear correlation coefficient to a given value.
}
\usage{
cor.conf(var1, var2, theo)
}
\arguments{
  \item{var1}{
numeric vector (first variable).
}
  \item{var2}{
numeric vector (second variable).
}
  \item{theo}{
theoretical value.
}
}
\value{
  \item{r.theo}{theoretical coefficient.}
  \item{r.obs}{observed coefficient.}
  \item{u}{test statistics.}
  \item{p}{p-value.}
  \item{conform}{data frame of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{cor.test}}
}
\examples{
var1<-1:30+rnorm(30,0,4)
var2<-1:30+rnorm(30,0,4)
cor.conf(var1,var2,theo=0.5)
}