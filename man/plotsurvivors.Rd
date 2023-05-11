\name{plotsurvivors}
\alias{plotsurvivors}
\title{
Survivor curve
}
\description{
Plots the survivor curve (log(survivors) against time) of a dataset to check for constancy of hazard.
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
\value{
  \item{n}{initial number of individuals.}
  \item{time}{time of events.}
  \item{alive}{number of survivors at each time.}
}
\author{
Maxime HERVE <maxime.herve@univ-rennes1.fr>
}
\examples{
# 'kidney' dataset of package 'survival'
require(survival)
data(kidney)
plotsurvivors(kidney$time,kidney$status)
}