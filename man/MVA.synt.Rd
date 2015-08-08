\name{MVA.synt}
\alias{MVA.synt}
\title{
Synthesis quality of multivariate analyses
}
\description{
Gives a simple estimator of the quality of the (descriptive) synthesis performed by a wide range of multivariate analyses.
}
\usage{
MVA.synt(x, rows = 5)
}
\arguments{
  \item{x}{
  a multivariate analysis (see Details).
}
  \item{rows}{
  maximum number of axes to print in the output.
}
}
\details{
Many multivariate analyses are supported, from various packages. The list will progressively get longer (and additional criteria will be given).

- PCA: \code{\link{prcomp}}, \code{\link{princomp}}, \code{\link[ade4]{dudi.pca}}, \code{\link[vegan]{rda}}, \code{\link[mixOmics]{pca}}, \code{\link[labdsv]{pca}}: \% of total variance explained by each axis.

- sPCA: \code{\link[mixOmics]{spca}}: \% of total variance explained by each axis.

- IPCA: \code{\link[mixOmics]{ipca}}: kurtosis of each axis.

- sIPCA: \code{\link[mixOmics]{sipca}}: kurtosis of each axis.

- PCoA: \code{\link{cmdscale}} (with \code{eig=TRUE}), \code{\link[ade4]{dudi.pco}}, \code{\link[vegan]{wcmdscale}} (with \code{eig=TRUE}), \code{\link[vegan]{capscale}}, \code{\link[labdsv]{pco}}, \code{\link[ape]{pcoa}}: \% of total variance explained by each axis.

- nMDS: \code{\link[vegan]{monoMDS}}, \code{\link[vegan]{metaMDS}}, \code{\link[labdsv]{nmds}}, \code{\link[MASS]{isoMDS}}: stress.

- LDA: \code{\link[MASS]{lda}}, \code{\link[ade4]{discrimin}}: \% of intergroup variance explained by each axis.

- PLS-DA (PLS2 on a dummy-coded factor): \code{\link[mixOmics]{plsda}}: \% of intergroup variance explained by each axis.

- CPPLS: \code{\link[pls]{mvr}}: \% of X and Y variances explained by each axis.

- PLSR: \code{\link[pls]{mvr}}, \code{\link[plsRglm]{plsR}}: \% of X and Y variances explained by each axis (only Y for the moment with \code{\link[plsRglm]{plsR}}).

- PCR: \code{\link[pls]{mvr}}: \% of X and Y variances explained by each axis.

- CDA: \code{\link[ade4]{discrimin}}, \code{\link[ade4]{discrimin.coa}}: \% of intergroup variance explained by each axis.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
data(iris)
PCA <- prcomp(iris[,1:4])
MVA.synt(PCA,"scores")
}