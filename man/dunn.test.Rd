\name{dunn.test}
\alias{dunn.test}
\title{
Dunn's test
}
\description{
Wrapper for \code{\link[dunn.test]{dunn.test}}, for more convenient result printing.
}
\usage{
dunn.test(resp, fact, p.method = "fdr", alpha = 0.05)
}
\arguments{
  \item{resp}{
response vector.
}
  \item{fact}{
grouping factor.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link[dunn.test]{dunn.test}}. Names \code{"fdr"}, \code{"BH"} and \code{"BY"} (as used with the classic \link{p.adjust}) are also allowed.
}
  \item{alpha}{
the nominal level of significance used in the step-up/step-down multiple comparisons procedures (see help of \code{\link[dunn.test]{dunn.test}}).
}
}
\details{
See help of the \code{\link[dunn.test]{dunn.test}} function for more explanations.
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.value}{table of results.}
  \item{p.adjust.method}{method for p-values correction.}
}
\author{
Maxime Hervé <mx.herve@gmail.com> based on the function of Alexis Dinno
}
\seealso{
\code{\link[dunn.test]{dunn.test}}
}
\examples{
set.seed(1024)
response <- c(rpois(20,1),rpois(20,1.5),rpois(20,2.5))
fact <- gl(3,20,labels=LETTERS[1:3])
kruskal.test(response~fact)
dunn.test(response,fact)
}