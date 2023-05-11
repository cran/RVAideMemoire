\name{test.multinom}
\alias{test.multinom}
\title{
Significance tests of coefficients (multinomial regression)
}
\description{
Tests for significance of coefficients associated with a given predictor of a model fitted with \code{\link[nnet]{multinom}}. Wald tests are used. All coefficients are generated and tested through the building of models using different reference classes (for the response but also for qualitative predictors with more than 2 levels).
}
\usage{
test.multinom(model, variable)
}
\arguments{
  \item{model}{
object of class \code{"multinom"}.
}
  \item{variable}{
any predictor present in \code{model} (unquoted).
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}