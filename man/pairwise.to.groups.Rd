\name{pairwise.to.groups}
\alias{pairwise.to.groups}
\title{
Letter summary of similarities and differences
}
\description{
Converts a pairwise-comparison matrix into a character-based display in which common characters identify levels or groups that are not significantly different. The function is based on \code{\link[multcompView]{multcompLetters}}.
}
\usage{
pairwise.to.groups(pairwise.test, component = "p.value", alpha = 0.05)
}
\arguments{
  \item{pairwise.test}{
a list, typically of class \code{"pairwise.htest"}.
}
  \item{component}{
name of the component of \code{pairwise.test} containing the pairwise-comparison matrix. The default \code{"p.value"} corresponds to all objects of class \code{"pairwise.htest"}.
}
  \item{alpha}{
significance threshold.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[multcompView]{multcompLetters}} for confidence intervals (and tests).
}
\examples{
attach(airquality)
Month <- factor(Month,labels=month.abb[5:9])
(test <- pairwise.t.test(Ozone, Month))
pairwise.to.groups(test)
detach()
}
