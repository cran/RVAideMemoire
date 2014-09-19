\name{cor.sparse}
\alias{cor.sparse}
\title{
Correlation between variables and components in sparse multivariate analyses
}
\description{
Calculate correlation between variables and components in sparse multivariate analyses (either obtained from \code{\link[mixOmics]{spca}}, \code{\link[mixOmics]{sipca}} or \code{\link[mixOmics]{splsda}}).
}
\usage{
cor.sparse(x)
}
\arguments{
  \item{x}{
object of class \code{"spca"} (from \code{\link[mixOmics]{spca}}), \code{"sipca"} (from \code{\link[mixOmics]{sipca}}) or \code{"splsda"} (from \code{\link[mixOmics]{splsda}}).
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[mixOmics]{spca}}, \code{\link[mixOmics]{sipca}}, \code{\link[mixOmics]{plsda}}
}
\examples{
# sPCA model
require(mixOmics)
data(liver.toxicity)
spca.rat <- spca(liver.toxicity$gene,ncomp=2,keepX=rep(20,2))
cor.sparse(spca.rat)
}
