\name{rating.lsmeans}
\alias{rating.lsmeans}
\title{
LSMeans for Cumulative Link (Mixed) Models
}
\description{
Extracts LSMeans (produced by \code{\link[lsmeans]{lsmeans}}) from Cumulative Link (Mixed) Models (produced by \code{\link[ordinal]{clm}} or \code{\link[ordinal]{clmm}}), with different possible formats.
}
\usage{
rating.lsmeans(lsm, type = c("prob", "cumprob", "class1", "class2"), level = 0.9)
}
\arguments{
  \item{lsm}{
object returned by \code{\link[lsmeans]{lsmeans}} applied on a \code{\link[ordinal]{clm}} or \code{\link[ordinal]{clmm}} object.
}
  \item{type}{
type of output to be returned: \code{"prob"} (default) gives probability of each rating, \code{"cumprob"} gives cumulative probabilities (Pi is probability to be <= to rating i), \code{"class1"} gives the most probable rating and \code{"class2"} gives the first rating for which the cumulative probability is >= to \code{level}.
}
  \item{level}{
used only for type \code{"class2"} (see \code{type}).
}
}
\details{
A factor named \code{cut} must have been called in \code{lsmeans}, to compute LSMeans per cut point (i.e. rating). Additionally, the argument \code{mode} of \code{lsmeans} must have been set to "linear.predictor". Finally, the call to \code{lsmeans} is typically like \code{lsmeans(model,~factor|cut,mode="linear.predictor")} where \code{factor} is the factor (or interaction) giving levels for which LSMeans have to be computed.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[lsmeans]{lsmeans}}, \code{\link[ordinal]{clm}}, \code{\link[ordinal]{clmm}}
}
\examples{
require(ordinal)
require(lsmeans)

model <- clm(rating~contact*temp,data=wine)
LSM <- lsmeans(model,~contact:temp|cut,mode="linear.predictor")

# Probabilities
rating.lsmeans(LSM)

# Cumulative probabilities
rating.lsmeans(LSM,type="cumprob")

# Most probable rating
rating.lsmeans(LSM,type="class1")
}