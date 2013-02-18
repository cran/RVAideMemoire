\name{adjust.esticon}
\alias{adjust.esticon}
\title{
'esticon()' results with corrected p-values
}
\description{
Return results of the \code{esticon()} function (package doBy), except that p-values are corrected.
}
\usage{
adjust.esticon(model, mat, p.method = "fdr", ...)
}
\arguments{
  \item{model}{
model. See help of the \code{esticon()} function.
}
  \item{mat}{
matrix of comparisons (contrasts). See help of the \code{esticon()} function.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
  \item{\dots}{
other arguments. See help of the \code{esticon()} function.
}
}
\value{
  \item{method}{name of the test.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[doBy]{esticon}}
}