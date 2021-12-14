\name{p.beta}
\alias{p.beta}
\title{
Rescaling of a [0,1] variable into the (0,1) interval (and vice-versa)
}
\description{
The function uses the formula presented in Douma & Weedon (2019). It is primarily intended to be used in beta regression (regression for continuous proportions) when data contain zeroes and/or ones, but can be applied to any variable initially bounded in the [0,1] interval when rescaling is necessary. The function can also perform back-transformation.
}
\usage{
p.beta(p, n = length(p), C = 2, back = FALSE)
}
\arguments{
  \item{p}{
numeric vector of values in the [0,1] interval.
}
  \item{n}{
total number of observations in the initial data set. Not very useful when the transformation is applied to the initial data set, but needed when back-transformation is applied from predicted values.
}
  \item{C}{
number of categories from which \code{p} was computed.
}
  \item{back}{
logical. If \code{TRUE}, back-transformation is performed rather than transformation.
}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr> from the following paper:
Douma JC & Weedon JT (2019) Analysing continuous proportions in ecology and evolution: A practical introduction to beta and Dirichlet regression. Methods in Ecology and Evolution, 10: 1412-1430
}
\examples{
# A fictive example with four animals performing a behavioral choice-test where time
#   can be spent in three branches (total time 20 min)
(tab <- data.frame(Individual=c("Ind1","Ind2","Ind3","Ind4"),Branch1=c(0,12,20,4),
  Branch2=c(8,4,0,6),Branch3=c(12,4,0,10)))
# Raw proportions of time spent in branch 1:
(p1 <- tab$Branch1/rowSums(tab[,-1]))
# Scaled proportions:
p.beta(p1,C=3)
}