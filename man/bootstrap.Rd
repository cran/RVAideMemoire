\name{bootstrap}
\alias{bootstrap}
\title{
Bootstrap
}
\description{
Simplified version of the \code{\link[boot]{boot}} function.
}
\usage{
bootstrap(x, fun, nrep = 1000, conf.level = 0.95, ...)
}
\arguments{
  \item{x}{
numeric vector.
}
  \item{fun}{
function to be used for computation (\code{function(x,i) ...(x[i])}).
}
  \item{nrep}{
number of replicates.
}
  \item{conf.level}{
confidence level for confidence interval.
}
  \item{\dots}{
additional arguments to \code{\link[boot]{boot}}. See help of this function.
}
}
\details{
See help of the \code{\link[boot]{boot}} function for more explanations.
}
\value{
  \item{method}{the character string \code{"Bootstrap"}}
  \item{data.name}{a character string giving the name of the data.}
  \item{estimate}{the estimated original value}
  \item{conf.level}{confidence level for confidence interval.}
  \item{rep}{number of replicates.}
  \item{conf.int}{limits of the confidence interval.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[boot]{boot}}
}
\examples{
# Confidence interval of a mean
samp <- sample(1:50,10,replace=TRUE)
bootstrap(samp,function(x,i) mean(x[i]))

# Confidence interval of the standard error of the mean
bootstrap(samp,function(x,i) sd(x[i])/sqrt(length(x[i])))
}
