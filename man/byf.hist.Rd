\name{byf.hist}
\alias{byf.hist}
\title{
Histogram for factor levels
}
\description{
Draw a histogram of a numeric variable per level of a factor.
}
\usage{
byf.hist(x, f, ...)
}
\arguments{
  \item{x}{
numeric vector.
}
  \item{f}{
factor.
}
  \item{...}{
other arguments to pass to the \code{hist()} function.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
data(iris)
byf.hist(iris$Sepal.Length,iris$Species)
}