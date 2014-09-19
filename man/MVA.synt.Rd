\name{MVA.synt}
\alias{MVA.synt}
\title{
Synthesis quality of multivariate analyses
}
\description{
Give a simple estimator of the quality of the (descriptive) synthesis performed by a wide range of multivariate analyses. See 'Details' for a list of analyses covered. 
}
\usage{
MVA.synt(x)
}
\arguments{
  \item{x}{
an object of class \code{"dudi.pca"}, \code{"prcomp"}, \code{"princomp"}, \code{"spca"}, \code{"pcoa"}, \code{"dudi.pco"}, \code{"pco"}, \code{"nmds"}, \code{"monoMDS"}, \code{"metaMDS"}, \code{"dudi.coa"}, \code{"lda"}, \code{"discrimin"} or \code{"plsda"} (see 'Details').
}
}
\details{
Total variance explained by axes is given for PCA (performed using \code{\link[ade4]{dudi.pca}}, \code{\link{prcomp}}, \code{\link[mixOmics]{pca}} or \code{\link{princomp}}), sPCA (performed using \code{\link[mixOmics]{spca}}), PCoA (performed using \code{\link[ape]{pcoa}}, \code{\link[ade4]{dudi.pco}} or \code{\link[labdsv]{pco}}) and COA (performed using \code{\link[ade4]{dudi.coa}}).

Intergroup variance explained by axes is given for LDA (performed using \code{\link[MASS]{lda}} or \code{\link[ade4]{discrimin}}), PLS-DA (performed using \code{\link[mixOmics]{plsda}}) and CDA (performed using \code{\link[ade4]{discrimin.coa}}).

Global stress is given for NMDS (performed using \code{\link[labdsv]{nmds}}, \code{\link[vegan]{monoMDS}} or \code{\link[vegan]{metaMDS}}).

Note to users of \code{\link{cmdscale}}: result of the wrapper \code{\link[labdsv]{pco}} is covered by \code{MVA.synt}.

Note to users of \code{\link[MASS]{isoMDS}}: result of the wrapper \code{\link[labdsv]{nmds}} is covered by \code{MVA.synt}.
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[ade4]{dudi.pca}}, \code{\link{prcomp}}, \code{\link[mixOmics]{pca}}, \code{\link{princomp}}, \code{\link[mixOmics]{spca}}, \code{\link[ape]{pcoa}}, \code{\link[ade4]{dudi.pco}}, \code{\link[labdsv]{pco}}, \code{\link[ade4]{dudi.coa}}, \code{\link[MASS]{lda}}, \code{\link[ade4]{discrimin}}, \code{\link[mixOmics]{plsda}}, \code{\link[ade4]{discrimin.coa}}, \code{\link[labdsv]{nmds}}, \code{\link[vegan]{monoMDS}}, \code{\link[vegan]{metaMDS}}
}
