\name{GPA.test}
\alias{GPA.test}
\title{
Significance test for GPA
}
\description{
Performs a permutation significance test based on total variance explained for Generalized Procrustes Analysis. The function uses \code{\link[FactoMineR]{GPA}}.
}
\usage{
GPA.test(df, group, tolerance = 10^-10, nbiteration = 200, scale = TRUE,
  nperm = 999, progress = TRUE)
}
\arguments{
  \item{df}{
a data frame with n rows (individuals) and p columns (quantitative varaibles), in which all data frames are combined.
}
  \item{group}{
a vector indicating the number of variables in each group (i.e. data frame).
}
  \item{tolerance}{
a threshold with respect to which the algorithm stops, i.e. when the difference between the GPA loss function at step n and n+1 is less than \code{tolerance}.
}
  \item{nbiteration}{
the maximum number of iterations until the algorithm stops.
}
  \item{scale}{
logical, if \code{TRUE} (default) scaling is required.
}
  \item{nperm}{
number of permutations.
}
  \item{progress}{
logical indicating if the progress bar should be displayed.
}
}
\details{
Rows of each data frame are randomly and independently permuted.

The function deals with the limitted floating point precision, which can bias calculation of p-values based on a discrete test statistic distribution.
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data, plus additional information.}
  \item{statistic}{the value of the test statistics.}
  \item{permutations}{the number of permutations.}
  \item{p.value}{the p-value of the test.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\references{
Wakeling IN, Raats MM and MacFie HJH (1992) A new significance test for consensus in Generalized Procrustes Analysis. Journal of Sensory Studies 7:91-96.
}
\seealso{
\code{\link[FactoMineR]{GPA}}
}
\examples{
require(FactoMineR)
data(wine)

\dontrun{GPA.test(wine[,-(1:2)],group=c(5,3,10,9,2))}
}
