\name{rating.emmeans}
\alias{rating.emmeans}
\title{
EMMeans for Cumulative Link (Mixed) Models
}
\description{
Extracts EMMeans (produced by \code{\link[emmeans]{emmeans}}) from Cumulative Link (Mixed) Models (produced by \code{\link[ordinal]{clm}} or \code{\link[ordinal]{clmm}}), with different possible formats.
}
\usage{
rating.emmeans(emm, type = c("prob", "cumprob", "class1", "class2"), level = 0.9)
}
\arguments{
  \item{emm}{
object returned by \code{\link[emmeans]{emmeans}} applied on a \code{\link[ordinal]{clm}} or \code{\link[ordinal]{clmm}} object.
}
  \item{type}{
type of output to be returned: \code{"prob"} (default) gives probability of each rating, \code{"cumprob"} gives cumulative probabilities (Pi is probability to be <= to rating i), \code{"class1"} gives the most probable rating and \code{"class2"} gives the first rating for which the cumulative probability is >= to \code{level}.
}
  \item{level}{
used only for type \code{"class2"} (see \code{type}).
}
}
\details{
A factor named \code{cut} must have been called in \code{emmeans}, to compute EMMeans per cut point (i.e. rating). Additionally, the argument \code{mode} of \code{emmeans} must have been set to "linear.predictor". Finally, the call to \code{emmeans} is typically like \code{emmeans(model,~factor|cut,mode="linear.predictor")} where \code{factor} is the factor (or interaction) giving levels for which EMMeans have to be computed.
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[emmeans]{emmeans}}, \code{\link[ordinal]{clm}}, \code{\link[ordinal]{clmm}}
}
\examples{
require(ordinal)
require(emmeans)

model <- clm(rating~contact*temp,data=wine)
EMM <- emmeans(model,~contact:temp|cut,mode="linear.predictor")

# Probabilities
rating.emmeans(EMM)

# Cumulative probabilities
rating.emmeans(EMM,type="cumprob")

# Most probable rating
rating.emmeans(EMM,type="class1")
}