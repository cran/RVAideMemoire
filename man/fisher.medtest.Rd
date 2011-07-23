\name{fisher.medtest}
\alias{fisher.medtest}
\title{
Comparison of 2 medians by Fisher's exact test
}
\description{
Perform a Fisher's exact test to compare 2 medians whith small unpaired samples.
}
\usage{
fisher.medtest(x, y)
}
\arguments{
  \item{x}{
numeric vector.
}
  \item{y}{
numeric vector.
}
}
\value{
  \item{datas}{variables name.}
  \item{median}{median of the whole sample.}
  \item{tab.cont}{contingency table used to perform the test.}
  \item{odds.ratio}{test statistics.}
  \item{p.value}{p-value of the test.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{fisher.test}}
}
\examples{
x<-rnorm(4,3,1.5)
y<-rnorm(4,5.5,2)
fisher.medtest(x,y)
}