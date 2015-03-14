\name{chisq.bin.exp}
\alias{chisq.bin.exp}
\title{
Expected counts for comparison of response probabilities to given values
}
\description{
Returns expected counts before comparing response probabilities (i.e. when the response variable is a binary variable) to given values by a chi-squared test. The function is in fact a wrapper to the chi-squared test for comparison of proportions to given values on a contingency table.
}
\usage{
chisq.bin.exp(formula, data, p, graph = FALSE)
}
\arguments{
  \item{formula}{
a formula of the form \code{a ~ b}, where \code{a} and \code{b} give the data values and corresponding groups, respectively. \code{a} can be a numeric vector or a factor, with only two possible values (except NA).
}
  \item{data}{
an optional data frame containing the variables in the formula \code{formula}. By default the variables are taken from \code{environment(formula)}.
}
  \item{p}{
theoretical probabilities.
}
  \item{graph}{
logical. If \code{TRUE} a mosaic plot of expected counts is drawn.
}
}
\details{
The function returns how many counts can be < 5 to respect Cochran's rule (80\% of counts must be >= 5).
}
\value{
  \item{p.theo}{theoretical probabilities.}
  \item{mat}{contingency table of expected counts.}
  \item{cochran}{number of counts which can be < 5.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{prop.test}}, \code{\link{chisq.theo.bintest}}, \code{\link{mosaicplot}}
}
\examples{
response <- c(rep(0:1,c(40,60)),rep(0:1,c(55,45)),rep(0:1,c(65,35)))
fact <- gl(3,100,labels=LETTERS[1:3])
p.theo <- c(0.5,0.45,0.2)
chisq.bin.exp(response~fact,p=p.theo)
}
