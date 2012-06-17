\name{plotresid}
\alias{plotresid}
\title{
Simple analysis of model residuals
}
\description{
Plot residuals of a model against fitted values and a QQplot of these residuals. Optionally, a Shapiro-Wilk's test can be performed on residuals. The function deals with \code{lm} (including \code{glm} and \code{glm.nb}), \code{mer}, \code{glmmadmb}, \code{nls}, \code{survreg} and \code{least.rect} models. Quantile residuals (function \code{qresiduals()},package statmod) are used for \code{glm.nb} models and for \code{glm} models with a poisson, quasipoisson, binomial or quasibinomial family.
}
\usage{
plotresid(model, shapiro = FALSE)
}
\arguments{
  \item{model}{
\code{lm}, \code{mer}, \code{glmmadmb}, \code{nls}, \code{survreg} or \code{least.rect} model.
}
  \item{shapiro}{
logical. If \code{TRUE} a Shapiro-Wilk's test is performed on residuals.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link{nls}}, \code{\link[survival]{survreg}}, \code{\link[RVAideMemoire]{least.rect}}, \code{\link[statmod]{qresiduals}}, \code{\link{qqnorm}}
}
