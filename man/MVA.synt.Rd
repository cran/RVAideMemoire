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
Many multivariate analyses are supported, from various packages.

- PCA: \code{\link{prcomp}}, \code{\link{princomp}}, \code{\link[ade4]{dudi.pca}}, \code{\link[vegan]{rda}}, \code{\link[mixOmics]{pca}}, \code{\link[labdsv]{pca}}: \% of total variance explained by each axis.

- sPCA: \code{\link[mixOmics]{spca}}: \% of total variance explained by each axis.

- IPCA: \code{\link[mixOmics]{ipca}}: kurtosis of each axis.

- sIPCA: \code{\link[mixOmics]{sipca}}: kurtosis of each axis.

- PCoA: \code{\link{cmdscale}} (with \code{eig=TRUE}), \code{\link[ade4]{dudi.pco}}, \code{\link[vegan]{wcmdscale}} (with \code{eig=TRUE}), \code{\link[vegan]{capscale}}, \code{\link[labdsv]{pco}}, \code{\link[ape]{pcoa}}: \% of total variance explained by each axis.

- nMDS: \code{\link[vegan]{monoMDS}}, \code{\link[vegan]{metaMDS}}, \code{\link[labdsv]{nmds}}, \code{\link[MASS]{isoMDS}}: stress.

- RDA: \code{\link[ade4]{pcaiv}}, \code{\link[ade4]{pcaivortho}}, \code{\link[vegan]{rda}}: \% of constrained and unconstrained total variance, \% of constrained variance explained by constrained axes (\code{\link[ade4]{pcaiv}} and \code{\link[vegan]{rda}}), \% of unconstrained variance explained by unconstrained axes (\code{\link[ade4]{pcaivortho}} and \code{\link[vegan]{rda}}).

- db-RDA (or CAP): \code{\link[vegan]{capscale}}, \code{\link[vegan]{dbrda}}:  \% of constrained and unconstrained total variance, \% of constrained variance explained by constrained axes, \% of unconstrained variance explained by unconstrained axes.

- COA: \code{\link[ade4]{dudi.coa}}, \code{\link[vegan]{cca}}: \% of total inertia explained by each axis.

- CCA: \code{\link[ade4]{cca}}, \code{\link[vegan]{cca}}: \% of constrained and unconstrained total inertia, \% of constrained inertia explained by constrained axes, \% of unconstrained inertia explained by unconstrained axes (\code{\link[vegan]{cca}} only).

- CPPLS: \code{\link[pls]{mvr}}: \% of X and Y variances explained by each axis.

- PLSR: \code{\link[pls]{mvr}}, \code{\link[plsRglm]{plsR}}: \% of X and Y variances explained by each axis (only Y for the moment with \code{\link[plsRglm]{plsR}}).

- 2B-PLS: \code{\link[mixOmics]{pls}}: \% of X/Y square covariance explained by each pair of axes, correlation between each pair of axes (canonical correlations).

- CCorA: \code{\link[vegan]{CCorA}}, \code{\link[mixOmics]{rcc}}: correlation between each pair of axes (canonical correlations).

- rCCorA: \code{\link[mixOmics]{rcc}}: correlation between each pair of axes (canonical correlations).

- PCR: \code{\link[pls]{mvr}}: \% of X and Y variances explained by each axis.

- MCA: \code{\link[ade4]{dudi.acm}}: \% of total inertia explained by each axis.

- Mix analysis: \code{\link[ade4]{dudi.mix}}, \code{\link[ade4]{dudi.hillsmith}}: \% of total inertia explained by each axis.

- GPA: \code{\link[FactoMineR]{GPA}}: \% of consensus and residual variance, \% of total variance exlained by each axis, \% of consensus variance explained by each axis, \% of residual variance coming from each group of variables.

- RGCCA: \code{\link[RGCCA]{rgcca}}, \code{\link[mixOmics]{wrapper.rgcca}}: \% of total intra-block variance explained by each axis, correlation between each pair of axes (canonical correlations).

- DIABLO: \code{\link[mixOmics]{block.plsda}}, \code{\link[mixOmics]{block.splsda}}: \% of total intra-block variance explained by each axis, correlation between each pair of axes (canonical correlations).

- CIA: \code{\link[ade4]{coinertia}}: RV coefficient, \% of co-inertia explained by each pair of axes, correlation between each pair of axes (canonical correlations).

- PCIA: \code{\link[ade4]{procuste}}: m2.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
data(iris)
PCA <- prcomp(iris[,1:4])
MVA.synt(PCA)
}