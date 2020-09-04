\name{prop.multinom}
\alias{prop.multinom}
\title{
Proportions and standard errors
}
\description{
Computes proportions (and their standard errors) when the number of classes is >= 2, based on predicted values of a model. The function is intended to be used parallel to a multinomial log-linear model.
}
\usage{
prop.multinom(x)
}
\arguments{
  \item{x}{
either a factor or a matrix with K columns giving the counts for each of the K classes.
}
}
\details{
The proportions can be computed through the \code{predict} function applied on a multinomial log-linear model (see \code{\link[nnet]{multinom}}). However, standard errors (or confidence intervals) cannot be obtained this way. The present function uses differents GLMs (in each case considering one category vs. the sum of all others) to obtain proportions and standard errors. Overdispersion is taken into account by default, using a quasibinomial law in all GLMs built.
}
\value{
  \item{probs}{the calculated proportions.}
  \item{se}{the calculated standard errors.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[nnet]{multinom}}, \code{\link{glm}}
}
\examples{
response <- data.frame(A=c(2,2,4,0,2,14,6,16,0,0),
			     B=c(2,0,0,0,6,2,10,6,0,0),
			     C=c(12,6,0,6,2,0,0,0,0,0),
			     D=c(0,0,0,14,0,0,0,0,2,0),
			     E=c(0,0,0,0,0,0,0,0,16,15))
prop.multinom(response)
}