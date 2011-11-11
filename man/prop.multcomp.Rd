\name{prop.multcomp}
\alias{prop.multcomp}
\title{
Pairwise comparisons after a test for given proportions
}
\description{
Perform pairwise comparisons after a global test for given proportions, using exact binomial tests.
}
\usage{
prop.multcomp(x, p, p.method = "fdr")
}
\arguments{
  \item{x}{
conitngency table.
}
  \item{p}{
theoretical proportions.
}
  \item{p.method}{
method for p-values correction. See help of the \code{p.adjust()} function.
}
}
\value{
  \item{observed}{observed proportions.}
  \item{expected}{expected proportions.}
  \item{p.method}{method for p-values correction.}
  \item{p.value}{corrected p-values.}
  \item{comp}{data frame or results.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{prop.test}}
}
\examples{
proportions<-sample(c(0,1),60,replace=TRUE)
populations<-sample(LETTERS[1:3],60,replace=TRUE)
tab.cont<-table(populations,proportions)
p.theo<-c(0.2,0.5,0.7)
prop.test(tab.cont,p=p.theo)
prop.multcomp(tab.cont,p=p.theo)
}