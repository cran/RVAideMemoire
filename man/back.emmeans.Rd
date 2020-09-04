\name{back.emmeans}
\alias{back.emmeans}
\title{
Back-transformation of EMMeans
}
\description{
Back-transforms EMMeans (produced by \code{\link[emmeans]{emmeans}}) when the model was built on a transformed response variable. This is typically the case when a LM(M) with \emph{log(x+1)} as response variable gives a better fitting than a GLM(M) for count data.
}
\usage{
back.emmeans(emm, transform = c("log", "logit", "sqrt", "4rt", "inverse"), base = exp(1),
  add = 0, ord = FALSE, decreasing = TRUE)
}
\arguments{
  \item{emm}{
object returned by \code{\link[emmeans]{emmeans}}.
}
  \item{transform}{
transformation applied to the response variable before building the model on which \code{emm} is based (\code{"4rt"} is fourth-root).
}
  \item{base}{
the base with respect to which the logarithm transformation was computed (if \code{transform="log"}). Defaults to \emph{e}=\code{exp(1)}.
}
  \item{add}{
value to be added to \emph{x} before computing the transformation, if needed (\emph{e.g.} \code{1} if the initial transformation was \emph{log(x+1)}).
}
  \item{ord}{
logical indicating if back-transformed EMMeans should be ordered.
}
  \item{decreasing}{
logical indicating in which order back-transformed EMMeans should be ordered, if \code{order=TRUE}.
}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[emmeans]{emmeans}}
}
\examples{
require(emmeans)

set.seed(1149)
tab <- data.frame(
  response <- c(rpois(30,0),rpois(30,2),rpois(30,4)),
  fact <- gl(3,30,labels=LETTERS[1:3])
)

model <- lm(log(response+1)~fact,data=tab)
EMM <- emmeans(model,~fact)
back.emmeans(EMM,transform="log",add=1)
}