\name{wald.ptheo.test}
\alias{wald.ptheo.test}
\title{
Wald test for comparison of a proportion to a theoretical value
}
\description{
Performs a Wald test for comparison of a proportion to a theoretical value.
}
\usage{
wald.ptheo.test(y, blocks = NULL, p = 0.5)
}
\arguments{
  \item{y}{
either a binary response (numeric vector or factor, with only two possible values except NA) or a two-column matrix with the columns giving the numbers of successes (left) and failures (right).
}
  \item{blocks}{
optional blocking (random) factor.
}
  \item{p}{
hypothesized probability of success.
}
}
\details{
The function builds a logistic (mixed) regression and applies a Wald test to compare the estimated value of the intercept to its theoretical value under H0. Eventual overdispersion is taken into account, by using a quasi-binomial law in case of no blocks or by introducing an individual-level random factor if blocks are present.

If the response is a 0/1 vector, the probability of the '1' group is tested. With other vectors, the response is transformed into a factor and the probability of the second level is tested.

If the response is a two-column matrix, the probability of the left column is tested.

If the reponse is a vector and no blocking factor is present, the exact binomial test performed by \code{\link{binom.test}} should be preferred since it is an exact test, whereas the Wald test is an approximate test.
}
\value{
  \item{method}{name of the test.}
  \item{data.name}{a character string giving the name(s) of the data.}
  \item{statistic}{test statistics of the test.}
  \item{p.value}{p-value of the test.}
  \item{estimate}{the estimated proportion (calculated without taking into account the blocking factor, if present).}
  \item{alternative}{a character string describing the alternative hypothesis, always \code{"two.sided"}.}
  \item{null.value}{the value of the proportion under the null hypothesis.}
  \item{parameter}{the degrees of freedom for the t-statistic, only whith overdispersion and no blocks.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\seealso{
\code{\link{binom.test}}, \code{\link{glm}}, \code{\link[lme4]{glmer}}
}
\examples{
set.seed(2006)
response <- sample(0:1,60,replace=TRUE)

# Comparison to p=0.5
wald.ptheo.test(response)

# Comparison to p=0.8
wald.ptheo.test(response,p=0.8)

# With a blocking factor

require(lme4)
blocks <- gl(3,20)
wald.ptheo.test(response,blocks)
}