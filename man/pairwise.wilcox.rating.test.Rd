\name{pairwise.wilcox.rating.test}
\alias{pairwise.wilcox.rating.test}
\title{
Pairwise comparisons for ratings using Wilcoxon rank sum tests
}
\description{
Perform pairwise comparisons for ratings (ordinal response variables) between group levels with correction for multiple testing.
}
\usage{
pairwise.wilcox.rating.test(x, g, p.adjust.method = "fdr",
  paired = FALSE, ...)
}
\arguments{
 \item{x}{
response vector (preferably an ordered factor).
}
  \item{g}{
grouping factor.
}
  \item{p.adjust.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
  \item{paired}{
  a logical indicating whether you want a paired test.
}
  \item{...}{
  additional arguments to be passed to \code{\link[RVAideMemoire]{wilcox.rating.test}}
}
}
\value{
Object of class \code{"pairwise.htest"}
}
\seealso{
\code{\link[RVAideMemoire]{wilcox.rating.test}}
}
\examples{
set.seed(1609)
response <- factor(sample(1:6,30,replace=TRUE),levels=c("1","2","3","4","5","6"),ordered=TRUE)
fact <- gl(3,1,30,labels=LETTERS[1:3])
kruskal.rating.test(response~fact)
pairwise.wilcox.rating.test(response,fact)
}
