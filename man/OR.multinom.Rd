\name{OR.multinom}
\alias{OR.multinom}
\title{
Odds-ratio (multinomial regression)
}
\description{
Computes the odds ratios and their confidence interval for a predictor of a model fitted with \code{\link[nnet]{multinom}}.
}
\usage{
OR.multinom(model, variable, conf.level = 0.95)
}
\arguments{
  \item{model}{
object of class \code{"multinom"}.
}
  \item{variable}{
any predictor present in \code{model}.
}
  \item{conf.level}{
confidence level.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}