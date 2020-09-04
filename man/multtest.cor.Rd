\name{multtest.cor}
\alias{multtest.cor}
\alias{plot.multtest.cor}
\title{
Univariate correlation test for multiple variables
}
\description{
Performs correlation tests between one variable and a series of other variables, and corrects p-values.
}
\usage{
multtest.cor(mult.var, uni.var, method = "pearson", p.method = "fdr",
  ordered = TRUE)

\method{plot}{multtest.cor}(x, arrows = TRUE, main = NULL, pch = 16,
  cex = 1, col = c("red", "orange", "black"), labels = NULL, ...)
}
\arguments{
  \item{mult.var}{
data frame containing a series of numeric variables.
}
  \item{uni.var}{
numeric variable (vector).
}
  \item{method}{
a character string indicating which correlation coefficient is to be computed. See help of \code{\link{cor}}.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{ordered}{
logical indicating if variables should be ordered based on correlation values.
}
  \item{x}{
object returned from \code{multtest.cor}.
}
  \item{arrows}{
logical indicating if arrows should be plotted. If \code{FALSE}, points are displayed at the extremity of the arrows.
}
  \item{main}{
optional title of the graph.
}
  \item{pch}{
symbol(s) used for points, when points are displayed (see \code{arrows}).
}
  \item{cex}{
size of points and labels (see help of \code{\link{dotchart}}).
}
  \item{col}{
vector of three colors: first for variables with P < 0.05, second for variables with 0.05 < P < 0.1, third for variables with P > 0.1. Recycled if only one value.
}
  \item{labels}{
  names of the variables. If \code{NULL} (default), labels correspond to names found in \code{mult.var}.
}
  \item{\dots}{
not used.
}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{cor.test}}
}
\examples{
data(iris)

# Original coordinates
plot(Petal.Length~Sepal.Length,pch=16,col=as.numeric(iris$Species),data=iris)

# New axis
abline(-6,1.6)

# Coordinates on new axis
new.coord <- coord.proj(iris[,c("Sepal.Length","Petal.Length")],1.6)

# Correlation between the whole dataset and new coordinates
mult.cor <- multtest.cor(iris[,1:4],new.coord)
plot(mult.cor)
}