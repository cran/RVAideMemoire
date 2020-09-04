\name{MVA.plot}
\alias{MVA.plot}
\title{
Plotting of multivariate analyses
}
\description{
Displays several kinds of plots for multivariate analyses.
}
\usage{
MVA.plot(x, type = c("scores", "loadings", "correlations", "biplot", "pairs",
  "trajectories"), ...)
}
\arguments{
  \item{x}{
  a multivariate analysis (see Details).
}
  \item{type}{
  the type of plot to be displayed: score plot (default), loading plot, correlation circle, biplot, score plot showing paired samples or score plot showing trajectories, respectively.
}
  \item{...}{
  arguments to be passed to subfunctions. See Details.
}
}
\details{
Different subfunctions are used depending on the type of plot to be displayed: \code{\link[RVAideMemoire]{MVA.scoreplot}}, \code{\link[RVAideMemoire]{MVA.loadplot}}, \code{\link[RVAideMemoire]{MVA.corplot}}, \code{\link[RVAideMemoire]{MVA.biplot}}, \code{\link[RVAideMemoire]{MVA.pairplot}} or \code{\link[RVAideMemoire]{MVA.trajplot}}. These functions should not be used directly (everything can be done with the general \code{MVA.plot}) but for convenience, arguments and analyses supported are detailed in separate help pages.

Warning: the use of \code{attach} before running a multivariate analysis can prevent \code{MVA.plot} to get the values it needs, and make it fail.
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}