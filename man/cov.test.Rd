\name{cov.test}
\alias{cov.test}
\title{
Significance test for the covariance between two datasets
}
\description{
Performs a permutational test based on the sum of square covariance between variables of two datasets, to test wether the (square) covariance is higher than expected under random association between the two datasets. The test is relevent parallel to a 2B-PLS.
}
\usage{
cov.test(X, Y, scale.X = TRUE, scale.Y = TRUE, nperm = 999, progress = TRUE)
}
\arguments{
  \item{X}{
a numeric vector, matrix or data frame.
}
  \item{Y}{
a numeric vector, matrix or data frame.
}
  \item{scale.X}{
logical, if \code{TRUE} (default) scaling of \code{X} is required.
}
  \item{scale.Y}{
logical, if \code{TRUE} (default) scaling of \code{Y} is required.
}
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
}
\details{
The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data, plus additional information.}
  \item{statistic}{the value of the test statistics.}
  \item{permutations}{the number of permutations.}
  \item{p.value}{the p-value of the test.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}