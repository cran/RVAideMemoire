\name{byf.qqnorm}
\alias{byf.qqnorm}
\title{
QQ-plot for factor levels
}
\description{
Draw a QQ-plot of a numeric variable per level of a factor.
}
\usage{
byf.qqnorm(x, f, ...)
}
\arguments{
  \item{x}{
numeric vector.
}
  \item{f}{
factor.
}
  \item{...}{
other arguments to pass to the \code{qqnorm()} function.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
data(iris)
byf.qqnorm(iris$Sepal.Length,iris$Species)
}