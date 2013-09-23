\name{plot1comp.var}
\alias{plot1comp.var}
\title{
Plot of correlation between predictor variables and a single component
}
\description{
Represent correlations between predictor variables and a single axis (usually a factorial component).
}
\usage{
plot1comp.var(dfxy, axis = 1, cex = 1)
}
\arguments{
  \item{dfxy}{
a data frame containing correlation coefficients between predictor variables and axi(e)s.
}
  \item{axis}{
axis to be represented (the first by default).
}
  \item{cex}{
size of variables names.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\seealso{
\code{\link[ade4]{s.corcircle}}
}
\examples{
require(MASS)
data(iris)
LDA <- lda(iris[,1:4],iris$Species)
LDA.form <- LDA.format(LDA)
plot1comp.var(LDA.form$co)
}