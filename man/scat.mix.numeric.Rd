\name{scat.mix.numeric}
\alias{scat.mix.numeric}
\title{
Representation of numeric variables in Hill and Smith mix analysis
}
\description{
Represents numeric variables of a Hill and Smith mix analysis as in PCA. The function is based on \code{\link[ade4]{s.corcircle}}.
}
\usage{
scat.mix.numeric(obj.dudi, xax = 1, yax = 2, ...)
}
\arguments{
  \item{obj.dudi}{
\code{dudi.mix} object.
}
  \item{xax}{
horizontal axis.
}
  \item{yax}{
vertical axis.
}
  \item{\dots}{
additional arguments to \code{\link[ade4]{s.corcircle}}. See help of this function.
}
}
\references{
Champely, S. 2005 Introduction à l'analyse multivariée (factorielle) sous R.
}
\author{
Stéphane Champely
}
\seealso{
\code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{s.corcircle}}
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

amix <- dudi.hillsmith(tab,scannf=FALSE,nf=2)
scat.mix.numeric(amix)
}
