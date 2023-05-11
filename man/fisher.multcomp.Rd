\name{fisher.multcomp}
\alias{fisher.multcomp}
\title{
Pairwise comparisons using Fisher's exact test
}
\description{
Performs pairwise comparisons after a comparison of proportions or after a test for independence of 2 categorical variables, by using a Fisher's exact test.
}
\usage{
fisher.multcomp(tab.cont, p.method = "fdr")
}
\arguments{
  \item{tab.cont}{
contingency table.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
Since chi-squared and G tests are approximate tests, exact tests are preferable when the number of individuals is small (200 is a reasonable minimum).
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results of pairwise comparisons.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{chisq.test}}, \code{\link{prop.test}}, \code{\link{fisher.test}}
}
\examples{
# 2-column contingency table: comparison of proportions
tab.cont1 <- matrix(c(17,23,12,24,20,10),ncol=2,dimnames=list(c("Control",
  "Treatment1","Treatment2"),c("Alive","Dead")),byrow=TRUE)
fisher.test(tab.cont1)
fisher.multcomp(tab.cont1)

# 3-column contingency table: independence test
tab.cont2 <- as.table(matrix(c(25,10,12,6,15,14,9,16,9),ncol=3,dimnames=list(c("fair",
  "dark","russet"),c("blue","brown","green"))))
fisher.test(tab.cont2)
fisher.multcomp(tab.cont2)
}