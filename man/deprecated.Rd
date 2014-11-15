\name{deprecated}
\alias{DA.var}
\alias{fc.multcomp}
\alias{byf.normhist}
\title{
Deprecated functions in RVAideMemoire package
}
\description{
Functions that are not usable anymore, and will be entirely removed from the package in future versions.
}
\usage{
DA.var(...)
fc.multcomp(...)
byf.normhist(...)
}
\arguments{
  \item{...}{
previous arguments.
}
}
\details{
\code{DA.var} is replaced by the more generic \code{\link[RVAideMemoire]{MVA.synt}}.

\code{fc.multcomp} is not useful anymore since \code{\link[lsmeans]{lstrends}} (package lsmeans) does the same job in a much more powerful manner (see argument \code{var} of \code{\link[lsmeans]{lstrends}}).

\code{byf.normhist} was not very useful and \code{\link[RVAideMemoire]{byf.hist}} does nearly the same job.
}
