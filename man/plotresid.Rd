\name{plotresid}
\alias{plotresid}
\title{
Simple analysis of model residuals
}
\description{
Plot residuals of a model against fitted values, a QQplot of residuals and perform a Shapiro-Wilk test on residuals. The function deals with \code{lm} (including \code{glm} and \code{glm.nb}) and \code{mer} models. Quantile residuals (function \code{qresiduals()},package statmod) are used for \code{glm.nb} models and for \code{glm} models with a poisson, quasipoisson, binomial or quasibinomial family.
}
\usage{
plotresid(model)
}
\arguments{
  \item{model}{
\code{lm} or \code{mer} model.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[statmod]{qresiduals}}, \code{\link{qqnorm}}
}