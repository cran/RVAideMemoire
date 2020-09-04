\name{prop.multinom.test}
\alias{prop.multinom.test}
\title{
Wald tests for comparison of proportions
}
\description{
Performs pairwise comparisons of proportions when the number of classes is >= 2 with corrections for multiple testing.
}
\usage{
prop.multinom.test(x, p.method = "fdr")
}
\arguments{
  \item{x}{
either a factor or a matrix with K columns giving the counts for each of the K classes.
}
  \item{p.method}{
method for p-values correction. See help of \code{\link{p.adjust}}.
}
}
\details{
The function builds multinomial log-linear models (using \code{\link[nnet]{multinom}}) and applies Wald tests to compare the intercepts to 0. All necessary models (each time using a different reference level/class) are built to get p-values of all possible comparisons among levels/classes.
}
\value{
  \item{method}{a character string indicating the name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{p.adjust.method}{method for p-values correction.}
  \item{p.value}{table of results.}
  \item{z.tab}{table of z values.}
}
\author{
Maxime Hervé <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link[nnet]{multinom}}, \code{\link{binom.test}}
}
\examples{
response <- factor(rep(LETTERS[1:4],c(20,40,42,13)))
table(response)/length(response)
prop.multinom.test(response)
}