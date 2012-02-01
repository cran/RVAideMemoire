\name{psignif}
\alias{psignif}
\title{
Stars for p-values significance
}
\description{
Return a vector of symbols corresponding to p-values significance : 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
}
\usage{
psignif(p)
}
\arguments{
  \item{p}{
vector of p-values.
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}

\examples{
p <- c(0.005,0.02,0.0000001,0.35)
psignif(p)
}