\name{adjust.esticon}
\alias{adjust.esticon}
\title{
'esticon()' results with corrected p-values
}
\description{
Return the results of the \code{esticon()} function (package doBy), except that p-values are corrected.
}
\usage{
adjust.esticon(model, matrix, p.method = "fdr", ...)
}
\arguments{
  \item{model}{
model. See help of the \code{esticon()} function.
}
  \item{matrix}{
matrix of comparisons. See help of the \code{esticon()} function.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
  \item{\dots}{
other arguments. See help of the \code{esticon()} function.
}
}
\value{
  \item{tab.no.adjust}{results of the \code{esticon()} function.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{tab.adjust}{results with corrected p-values.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[doBy]{esticon}}
}