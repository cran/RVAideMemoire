\name{rating.prob}
\alias{rating.prob}
\title{
Observed rating frequencies
}
\description{
Computes observed rating frequencies per level of a factor, in various formats.
}
\usage{
rating.prob(x, g, type = c("prob", "cumprob", "class"))
}
\arguments{
  \item{x}{
ordered factor (ratings).
}
  \item{g}{
factor giving groups to be compared.
}
  \item{type}{
type of output to be returned: \code{"prob"} (default) gives frequency of each rating, \code{"cumprob"} gives cumulative frequencies (Fi is frequence of ratings <= i) and \code{"class"} gives the most frequent rating.
}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
require(ordinal)
data(wine)

# Frequencies
rating.prob(wine$rating,wine$contact:wine$temp)

# Cumulative frequencies
rating.prob(wine$rating,wine$contact:wine$temp,type="cumprob")

# Most frequent rating
rating.prob(wine$rating,wine$contact:wine$temp,type="class")
}