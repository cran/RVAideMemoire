\name{plotresid}
\alias{plotresid}
\title{
Simple analysis of model residuals
}
\description{
Plots residuals of a model against fitted values and a QQ-plot of these residuals. Optionally, a Shapiro-Wilk test can be performed on residuals. The function deals with \code{\link{lm}} (including \code{\link{glm}}, \code{\link[MASS]{glm.nb}}, \code{mlm} and \code{\link{manova}}), \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{glmmadmb}, \code{\link[nlme]{lme}}, \code{\link[nlme]{gls}}, \code{\link{nls}}, \code{\link[nlme]{nlsList}}, \code{\link[survival]{survreg}} and \code{\link[RVAideMemoire]{least.rect}} models. 
}
\usage{
plotresid(model, shapiro = FALSE)
}
\arguments{
  \item{model}{
an object of class \code{"lm"}, \code{"merMod"}, \code{"glmmadmb"}, \code{"lme"}, \code{"gls"}, \code{"nls"}, \code{"nlsList"}, \code{"survreg"} or \code{"least.rect"}.
}
  \item{shapiro}{
logical. If \code{TRUE} a Shapiro-Wilk test is performed on residuals.
}
}
\details{
Externally studentized resisuals are used for \code{\link{lm}}, \code{\link{glm}} (except with a poisson, quasipoisson, binomial or quasibinomial family). Standardized residuals are used for \code{\link[nlme]{nlme}}, \code{\link[nlme]{gls}}, \code{\link{nls}} and \code{\link[nlme]{nlsList}} models. Quantile residuals (function \code{\link[statmod]{qresiduals}}) are used for \code{\link[MASS]{glm.nb}}, \code{\link{glm}} (with a poisson, quasipoisson, binomial or quasibinomial family), \code{\link[lme4]{glmer}} (with a poisson or binomial family) and \code{\link[lme4]{glmer.nb}} models. In all other cases raw residuals are used.

With a \code{mlm} or \code{\link{manova}} model, only a multivariate QQ-plot is drawn. The test performed when \code{shapiro=TRUE} is a Shapiro-Wilk test for multivariate normality.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link{manova}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer.nb}}, \code{\link[nlme]{lme}}, \code{\link[nlme]{gls}}, \code{\link{nls}}, \code{\link[nlme]{nlsList}}, \code{\link[survival]{survreg}}, \code{\link[RVAideMemoire]{least.rect}}, \code{\link[statmod]{qresiduals}}, \code{\link[car]{qqPlot}}, \code{\link{shapiro.test}}, \code{\link[RVAideMemoire]{mqqnorm}}, \code{\link[RVAideMemoire]{mshapiro.test}}
}
