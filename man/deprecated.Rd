\name{deprecated}
\alias{DA.var}
\alias{fc.multcomp}
\alias{byf.normhist}
\alias{pairwise.manova}
\alias{wilcox.rating.signtest}
\alias{wilcox.rating.test}
\alias{kruskal.rating.test}
\alias{friedman.rating.test}
\alias{pairwise.wilcox.rating.test}
\alias{wilcox.paired.rating.multcomp}
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
pairwise.manova(...)
wilcox.rating.signtest(...)
wilcox.rating.test(...)
kruskal.rating.test(...)
friedman.rating.test(...)
pairwise.wilcox.rating.test(...)
wilcox.paired.rating.multcomp(...)
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

\code{pairwise.manova} is not useful anymore since \code{\link[lsmeans]{lsmeans}} (package lsmeans) does the same job in a much more powerful manner (on \code{"mlm"} objects, created by \code{\link{lm}} and not \code{\link{manova}})

\code{wilcox.rating.test}, \code{wilcox.rating.signtest}, \code{kruskal.rating.test}, \code{friedman.rating.test}, \code{pairwise.wilcox.rating.test} and \code{wilcox.paired.rating.multcomp} can be problematic with ratings (in which ties and zeroes are very frequent). The use of CLM(M)s (\emph{via} \code{\link[ordinal]{clm}} and \code{\link[ordinal]{clmm}}) is recommended.
}
