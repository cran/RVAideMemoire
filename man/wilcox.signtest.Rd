\name{wilcox.signtest}
\alias{wilcox.signtest}
\title{
Wilcoxon sign test
}
\description{
Perform a Wilcoxon sign test to compare medians of two small paired samples or one median to a given value.
}
\usage{
wilcox.signtest(x, data, mu = 0)
}
\arguments{
  \item{x}{
either a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor with 2 levels giving the corresponding groups (comparison of two medians); or a numeric vector (comparison of one median to a given value).
}
  \item{data}{
an optional data frame containing the variables in the formula \code{x} (if \code{x} is a formula). By default the variables are taken from \code{environment(x)}. Not used if \code{x} is a numeric vector.
}
  \item{mu}{
theoretical median.
}
}
\value{
  \item{method}{a character string indicating what type of test was performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{null.value}{the specified hypothesized value of the median or median difference depending on the test performed.}
  \item{p.value}{p-value of the test.}
  \item{estimate}{the estimated median or difference in medians depending on the test performed.}
  \item{alternative}{a character string giving the alternative hypothesis, always \code{"two.sided"}}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{wilcox.test}}
}
\examples{
set.seed(1706)
response <- c(rnorm(7,3,1.5),rnorm(7,5.5,2))

# Comparison of 2 samples
fact <- gl(2,7,labels=LETTERS[1:2])
wilcox.signtest(response~fact)

# Comparison to a given value
theo <- 4
wilcox.signtest(response,mu=theo)
}