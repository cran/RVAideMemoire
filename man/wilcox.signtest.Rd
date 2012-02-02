\name{wilcox.signtest}
\alias{wilcox.signtest}
\title{
Wilcoxon sign test
}
\description{
Perform a Wilcoxon sign test to compare medians of two small paired samples or one median to a given value.
}
\usage{
wilcox.signtest(x, y, mu = NULL)
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
  \item{method}{a character string indicating what type of test was performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
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
set.seed(1706)
x <- rnorm(7,3,1.5)

# Comparison of 2 samples
y <- rnorm(7,5.5,2)
wilcox.signtest(x,y)

# Comparison to a given value
theo <- 4
wilcox.signtest(x,mu=theo)
}