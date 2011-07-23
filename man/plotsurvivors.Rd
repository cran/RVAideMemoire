\name{plotsurvivors}
\alias{plotsurvivors}
\title{
Survivor curve
}
\description{
Plot the survivor curve (log(survivors) against time) of a dataset to check for constancy of hazard.
}
\usage{
plotsurvivors(x, status = rep(1, length(x)))
}
\arguments{
  \item{x}{
time to event.
}
  \item{status}{
status (1: event observed, 0: event not observed).
}
}
\author{
Maxime Hervé <mx.herve@gmail.com>
}
\examples{
# 'kidney' dataset in MASS package
data(kidney)
plotsurvivors(kidney$time,kidney$status)
}