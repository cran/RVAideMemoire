\name{wilcox.sign.test}
\alias{wilcox.sign.test}
\title{
Wilcoxon sign test
}
\description{
Perform a Wilcoxon sign test to compare medians of two small paired samples or one median to a given value.
}
\usage{
wilcox.sign.test(x, y, mu = NULL)
}
\arguments{
  \item{x}{
numeric vector (first sample).
}
  \item{y}{
optional numeric vector (second sample).
}
  \item{mu}{
theoretical median.
}
}
\value{
  \item{datas}{variable(s) name.}
  \item{mu}{theoretical median.}
  \item{p.value}{p-value of the test.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{wilcox.test}}
}
\examples{
x<-rnorm(7,3,1.5)
y<-rnorm(7,5.5,2)
wilcox.sign.test(x,y)

theo=4
wilcox.sign.test(x,mu=theo)
}