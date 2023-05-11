\name{scat.cr}
\alias{scat.cr}
\title{
"Correlation" of variables to axes in MCA or mix analyses
}
\description{
Represents the "correlation" of variables to axes in a MCA (from \code{\link[ade4]{dudi.acm}}) or a mix analysis (from \code{\link[ade4]{dudi.hillsmith}} or \code{\link[ade4]{dudi.mix}}).
}
\usage{
scat.cr(dudi.obj, axis = 1)
}
\arguments{
  \item{dudi.obj}{
object obtained from \code{\link[ade4]{dudi.acm}}, \code{\link[ade4]{dudi.hillsmith}} or \code{\link[ade4]{dudi.mix}}.
}
  \item{axis}{
axis to be represented (the first by default).
}
}
\details{
For quantitative variables, the squared correlation coefficient is displayed. For ordered factors, the squared multiple correlation coefficient is displayed. For unordered factors, the correlation ratio is displayed.
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>, based on an idea of Stephane Champely.
}
\seealso{
\code{\link[ade4]{dudi.acm}}, \code{\link[ade4]{dudi.hillsmith}}, \code{\link[ade4]{dudi.mix}}
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
tab <- data.frame(age,sex,size,weight,hand,eyes,hair,stringsAsFactors=TRUE)

amix <- dudi.hillsmith(tab,scannf=FALSE,nf=2)
scat.cr(amix)
}
