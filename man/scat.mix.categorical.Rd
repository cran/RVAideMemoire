\name{scat.mix.categorical}
\alias{scat.mix.categorical}
\title{
Representation of qualitative variables in Hill and Smith mix analysis
}
\description{
Represent qualitative variables of a Hill and Smith mix analysis as in MCA. The function is based on the \code{s.class()} function of the ade4 package.
}
\usage{
scat.mix.categorical(dudi.obj, xax = 1, yax = 2, csub = 2, possub = "topleft", ...)
}
\arguments{
  \item{dudi.obj}{
\code{dudi.mix} object.
}
  \item{xax}{
horizontal axis.
}
  \item{yax}{
vertical axis.
}
  \item{csub}{
title size.
}
  \item{possub}{
title position (\code{"topleft"}, \code{"topright"}, \code{"bottomleft"} or \code{"bottomright"}).
}
  \item{\dots}{
additional arguments to \code{s.class()}. See help of this function.
}
}
\references{
Champely, S. 2005 Introduction à l'analyse multivariée (factorielle) sous R.
}
\author{
Maxime Hervé <mx.herve@gmail.com>, based on a Stéphane Champely idea.
}
\seealso{
\code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{s.class}}
}
\examples{
require(ade4)
# Fictive dataset
age <- sample(15:60,50,replace=TRUE)
sex <- sample(c("M","F"),50,replace=TRUE)
size <- sample(155:190,50,replace=TRUE)
hair <- sample(c("Fair","Dark","Russet"),50,replace=TRUE)
eyes <- sample(c("Blue","Green","Brown"),50,replace=TRUE)
weight <- sample(50:85,50,replace=TRUE)
hand <- sample(c("Left.handed","Right.handed"),50,replace=TRUE)
tab <- data.frame(age,sex,size,weight,hand,eyes,hair)
amix <- dudi.mix(tab,scannf=FALSE,nf=2)
scat.mix.categorical(amix)
}
