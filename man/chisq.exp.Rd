\name{chisq.exp}
\alias{chisq.exp}
\title{
Expected counts for comparison of proportions to given values
}
\description{
Return expected counts before comparing proportions to given values by a chi-squared test.
}
\usage{
chisq.exp(data, p, graph = FALSE)
}
\arguments{
  \item{data}{
contingency table.
}
  \item{p}{
theoretical proportions.
}
  \item{graph}{
logical. If \code{TRUE} a mosaic plot of expected counts is drawn.
}
}
\details{
The function returns how many counts can be < 5 to respect Cochran's rule (80\% of counts must be >= 5).
}
\value{
  \item{p.theo}{theoretical proportions.}
  \item{mat}{contingency table of expected counts.}
  \item{cochran}{number of counts which can be < 5.}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link{prop.test}}, \code{\link{chisq.test}}, \code{\link{mosaicplot}}
}
\examples{
proportions <- sample(c(0,1),60,replace=TRUE)
populations <- sample(LETTERS[1:3],60,replace=TRUE)
tab.cont <- table(populations,proportions)
p.theo <- c(0.2,0.5,0.7)
chisq.exp(tab.cont,p=p.theo)
}
