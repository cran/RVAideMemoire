\name{byf.shapiro}
\alias{byf.shapiro}
\title{
Shapiro-Wilk's test for factor levels
}
\description{
Perform a Shapiro-Wilk's test on a numeric variable per level of a factor.
}
\usage{
byf.shapiro(x, f)
}
\arguments{
  \item{x}{
numeric vector.
}
  \item{f}{
factor.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
data(iris)
byf.shapiro(iris$Sepal.Length,iris$Species)
}