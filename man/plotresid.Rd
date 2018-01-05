\name{plotresid}
\alias{plotresid}
\title{
Simple analysis of model residuals
}
\description{
Plots residuals of a model against fitted values and for some models a QQ-plot of these residuals. Optionally, a Shapiro-Wilk test can be performed on residuals. The function deals with \code{\link{lm}} (including \code{\link{glm}}, \code{\link[nlme]{lmList}}, \code{\link[lme4]{lmList}}, \code{\link[MASS]{glm.nb}}, \code{mlm} and \code{\link{manova}}), \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[MASS]{glmmPQL}}, \code{glmmadmb}, \code{\link[nlme]{lme}}, \code{\link[nlme]{gls}}, \code{\link{nls}}, \code{\link[nlme]{nlsList}}, \code{\link[survival]{survreg}} and \code{\link[RVAideMemoire]{least.rect}} models. 
}
\usage{
plotresid(model, shapiro = FALSE)
}
\arguments{
  \item{model}{
an object of class \code{"lm"}, \code{"lmList"}, \code{"lmList4"}, \code{"merMod"}, \code{"glmmadmb"}, \code{"lme"}, \code{"glmmPQL"}, \code{"gls"}, \code{"nls"}, \code{"nlsList"}, \code{"survreg"} or \code{"least.rect"}.
}
  \item{shapiro}{
logical. If \code{TRUE} and if \code{model} is based on a Gaussian distribution, a Shapiro-Wilk test is performed on residuals.
}
}
\details{
Response residuals are used for linear models, non linear models and generalized linear models based on an identity link (except with \code{"quasi"} distributions where response residuals are used only if \code{variance="constant"}). Pearson or studentized residuals are used whenever there is a link function which is not identity (and with \code{"quasi"} distributions when \code{variance} is not \code{"constant"}).

QQ-plots and Shapiro-Wilk tests are available whenever the model is based on a Gaussian distribution (and with \code{"quasi"} distributions when \code{variance="constant"}).

With a \code{mlm} or \code{\link{manova}} model, only a multivariate QQ-plot is drawn. The test performed when \code{shapiro=TRUE} is a Shapiro-Wilk test for multivariate normality.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{lm}}, \code{\link[nlme]{lmList}}, \code{\link[lme4]{lmList}}, \code{\link{glm}}, \code{\link[MASS]{glm.nb}}, \code{\link{manova}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer.nb}}, \code{\link[nlme]{lme}}, \code{\link[MASS]{glmmPQL}}, \code{\link[nlme]{gls}}, \code{\link{nls}}, \code{\link[nlme]{nlsList}}, \code{\link[survival]{survreg}}, \code{\link[RVAideMemoire]{least.rect}}, \code{\link[statmod]{qresiduals}}, \code{\link[car]{qqPlot}}, \code{\link{shapiro.test}}, \code{\link[RVAideMemoire]{mqqnorm}}, \code{\link[RVAideMemoire]{mshapiro.test}}
}
