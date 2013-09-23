\name{overdisp.glmer}
\alias{overdisp.glmer}
\title{
Estimation of overdispersion with \code{\link[lme4]{glmer}} models
}
\description{
Estimate residual deviance and residual degrees of freedom to check for overdispersion with \code{\link[lme4]{glmer}} models. This function is directly comming from \code{http://glmm.wikidot.com/faq}.
}
\usage{
overdisp.glmer(model)
}
\arguments{
  \item{model}{
a model fitted by \code{\link[lme4]{glmer}}.
}
}
\author{
Ben Bolker
}
\seealso{
\code{\link[lme4]{glmer}}
}
\examples{
require(lme4)

# Example from the 'glmer' function
gm1 <- glmer(cbind(incidence,size-incidence)~period+(1|herd),
 family="binomial",data=cbpp)
overdisp.glmer(gm1)
}
