\name{fisher.multcomp}
\alias{fisher.multcomp}
\title{
Pairwise comparisons after a test for independence of 2 categorical variables
}
\description{
Perform pairwise comparisons after a test for independence of 2 categorical variables, by using a Fisher's exact test on each possible 2x2 table.
}
\usage{
fisher.multcomp(tab.cont, p.method = "fdr")
}
\arguments{
  \item{tab.cont}{
contingency table.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results of pairwise comparisons.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{chisq.test}}, \code{\link{fisher.test}}
}
\examples{
tab.cont <- as.table(matrix(c(25,10,12,6,15,14,9,16,9),ncol=3,dimnames=list(c("fair","dark","russet"),c("blue","brown","green"))))
chisq.test(tab.cont)
fisher.multcomp(tab.cont)
}