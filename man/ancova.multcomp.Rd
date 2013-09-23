\name{ancova.multcomp}
\alias{ancova.multcomp}
\title{
Pairwise comparisons after an analysis of covariance
}
\description{
Plot and return mean of the dependent variable for each class of the factor, adjusted for the covariate, and 95\% confidence interval of these means. The function deals with \code{\link{lm}}, \code{\link{glm}} and \code{\link[MASS]{glm.nb}} models.
}
\usage{
ancova.multcomp(model)
}
\arguments{
  \item{model}{
a \code{\link{lm}}, \code{\link{glm}} and \code{\link[MASS]{glm.nb}} model.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link[MASS]{glm.nb}}
}