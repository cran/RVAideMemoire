\name{multtest.gp.bin}
\alias{multtest.gp.bin}
\alias{plot.multtest.gp.bin}
\title{
Univariate comparison of groups for multiple binary variables
}
\description{
Performs group comparisons for multiple binary variables using a parametric or an exact test, and corrects p-values. Gives also group proportions and standards errors for each variable.
}
\usage{
multtest.gp.bin(tab, fac, test = c("LRT", "Fisher"),
  p.method = "fdr", ordered = TRUE, ...)

\method{plot}{multtest.gp.bin}(x, signif = FALSE, alpha = 0.05,
  vars = NULL, xlab = "Group", ylab = "Mean (+/- SE) proportion",
  titles = NULL, groups = NULL, ...)
}
\arguments{
  \item{tab}{
data frame containing response variables.
}
  \item{fac}{
factor defining groups to compare.
}
  \item{test}{
type of test to use: likelihood ratio test based on binomial GLM (\code{"LRT"}, default) or Fisher's exact test (\code{"Fisher"}).
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{ordered}{
logical indicating if variables should be ordered based on p-values.
}
  \item{x}{
object returned from \code{multtest.gp.bin}.
}
  \item{signif}{
logical indicating if only variables with significant P-value should be plotted.
}
  \item{alpha}{
significance threshold.
}
  \item{vars}{
numeric vector giving variables to plot (rows of \code{x}). Default to all, which can lead to errors if too many variables.
}
  \item{xlab}{
legend of the x axis.
}
  \item{ylab}{
legend of the y axis
}
  \item{titles}{
titles of the graphs (name of the variables by default).
}
  \item{groups}{
names of the bars (levels of \code{fac} by default).
}
  \item{\dots}{
additional arguments to testing functions in \code{multtest.gp.bin} (especially for \code{\link[car]{Anova}} and \code{\link{fisher.test}}) and to \code{\link{barplot}} in \code{plot}.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[RVAideMemoire]{multtest.gp}}, \code{\link[car]{Anova}}, \code{\link{fisher.test}}
}