\name{mod}
\alias{mod}
\title{
Mode
}
\description{
Renvoie le mode d'une série de données.
}
\usage{
mod(var)
}
\arguments{
  \item{var}{
série de données.
}
}
\author{
Maxime Hervé
}
\seealso{
\code{\link{density}}
}
\examples{
serie<-sample(1:50,30)
mod(serie)
}
