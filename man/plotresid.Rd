\name{plotresid}
\alias{plotresid}
\title{
Simple analysis of model residuals
}
\description{
Plot residuals of a model against fitted values, a QQplot of residuals and perform a Shapiro-Wilk test on residuals. The function deals with lm (including glm and glm.nb) and mer models. Quantile residuals (\code{qresiduals()} function, statmod package) are used for glm.nb models and for glm models with a poisson, quasipoisson, binomial or quasibinomial family.
}
\usage{
plotresid(model)
}
\arguments{
  \item{model}{
lm or mer model.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[statmod]{qresiduals}}, \code{\link{qqnorm}}
}