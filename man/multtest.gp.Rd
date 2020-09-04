\name{multtest.gp}
\alias{multtest.gp}
\alias{plot.multtest.gp}
\title{
Univariate comparison of groups for multiple variables
}
\description{
Performs group comparisons for multiple variables using parametric, permutational or rank tests, and corrects p-values. Gives also group means and standards errors for each variable.
}
\usage{
multtest.gp(tab, fac, test = c("param", "perm", "rank"),
  transform = c("none", "sqrt", "4rt", "log"), add = 0, p.method = "fdr",
  ordered = TRUE, ...)

\method{plot}{multtest.gp}(x, signif = FALSE, alpha = 0.05,
  vars = NULL, xlab = "Group", ylab = "Mean (+/- SE) value",
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
type of test to use: parametric (default), permutational (non parametric) or rank-based (non parametric). See Details.
}
  \item{transform}{
transformation to apply to response variables before testing (none by default; \code{"4rt"} is fourth-root). Only used for parametric and permutational tests.
}
  \item{add}{
value to add to response variables before a log-transformation.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{ordered}{
logical indicating if variables should be ordered based on p-values.
}
  \item{x}{
object returned from \code{multtest.gp}.
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
additional arguments to testing functions in \code{multtest.gp} (especially for \code{var.equal} in \code{\link{t.test}} and \code{nperm} in \code{\link[RVAideMemoire]{perm.anova}} and \code{\link[RVAideMemoire]{perm.t.test}}) and to \code{\link{barplot}} in \code{plot}.
}
}
\details{
In case of parametric tests, t-tests or ANOVAs are used depending on the number of groups (2 or more, respectively). In case of permutational tests: permutational t-tests or permutational ANOVAs. In case of rank-based tests: Mann-Whitney-Wilcoxon or Kruskal-Wallis tests.
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[RVAideMemoire]{perm.anova}}, \code{\link[RVAideMemoire]{perm.t.test}}
}
\examples{
data(iris)
mult <- multtest.gp(iris[,1:4],iris$Species)
plot(mult)
}