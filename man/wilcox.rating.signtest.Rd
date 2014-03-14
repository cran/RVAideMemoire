\name{wilcox.rating.signtest}
\alias{wilcox.rating.signtest}
\title{
Wilcoxon sign test for ratings
}
\description{
Wrapper for \code{\link[RVAideMemoire]{wilcox.signtest}} with ratings (ordinal response variables).
}
\usage{
wilcox.rating.signtest(x, data, mu = NULL)
}
\arguments{
  \item{x}{
either a formula of the form \code{a ~ b} where \code{a} gives the data values and \code{b} a factor with 2 levels giving the corresponding groups (comparison of two median ratings); or a numeric vector (comparison of one median rating to a given value). It is preferable that \code{a} (or \code{x}) is an ordered factor.
}
  \item{data}{
an optional data frame containing the variables in the formula \code{x} (if \code{x} is a formula). By default the variables are taken from \code{environment(x)}. Not used if \code{x} is a numeric vector.
}
  \item{mu}{
theoretical median rating.
}
}
\value{
See \code{\link[RVAideMemoire]{wilcox.signtest}}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{wilcox.rating.test}}
}
\examples{
set.seed(1706)
response <- factor(c(sample(1:4,7,replace=TRUE),sample(3:6,7,replace=TRUE)),
  levels=c("1","2","3","4","5","6"),ordered=TRUE)

# Comparison of 2 samples
fact <- gl(2,7,labels=LETTERS[1:2])
wilcox.rating.signtest(response~fact)

# Comparison to a given value
theo <- "4"
wilcox.rating.signtest(response,mu=theo)
}