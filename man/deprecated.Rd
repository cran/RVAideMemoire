\name{deprecated}
\alias{back.lsmeans}
\alias{byf.normhist}
\alias{cor.sparse}
\alias{DA.confusion}
\alias{DA.valid}
\alias{DA.var}
\alias{fc.multcomp}
\alias{friedman.rating.test}
\alias{kruskal.rating.test}
\alias{pairwise.manova}
\alias{pairwise.wilcox.rating.test}
\alias{plot1comp.ind}
\alias{plot1comp.var}
\alias{PLSDA.ncomp}
\alias{PLSDA.test}
\alias{rating.lsmeans}
\alias{s.corcircle2}
\alias{scat.mix.categorical}
\alias{scat.mix.numeric}
\alias{scatter.coa2}
\alias{wilcox.paired.rating.multcomp}
\alias{wilcox.rating.signtest}
\alias{wilcox.rating.test}
\title{
Deprecated functions in RVAideMemoire package
}
\description{
Functions that are not usable anymore, and will be entirely removed from the package in future versions.
}
\usage{
back.lsmeans(...)
byf.normhist(...)
cor.sparse(...)
DA.confusion(...)
DA.valid(...)
DA.var(...)
fc.multcomp(...)
friedman.rating.test(...)
kruskal.rating.test(...)
pairwise.manova(...)
pairwise.wilcox.rating.test(...)
plot1comp.ind(...)
plot1comp.var(...)
PLSDA.ncomp(...)
PLSDA.test(...)
rating.lsmeans(...)
s.corcircle2(...)
scat.mix.categorical(...)
scat.mix.numeric(...)
scatter.coa2(...)
wilcox.paired.rating.multcomp(...)
wilcox.rating.signtest(...)
wilcox.rating.test(...)
}
\arguments{
  \item{...}{
previous arguments.
}
}
\details{
\code{back.lsmeans} and \code{rating.lsmeans} are replaced by \code{\link[RVAideMemoire]{back.emmeans}} and \code{\link[RVAideMemoire]{rating.emmeans}}. More generally, stop using package lsmeans and change to package emmeans, its new version.

\code{byf.normhist} was not very useful and \code{\link[RVAideMemoire]{byf.hist}} does nearly the same job.

\code{cor.sparse} is replaced by the more generic \code{\link[RVAideMemoire]{MVA.plot}}.

\code{DA.confusion} and \code{DA.valid} are replaced by the more generic \code{\link[RVAideMemoire]{MVA.cmv}} and \code{\link[RVAideMemoire]{MVA.cv}}.

\code{DA.var} is replaced by the more generic \code{\link[RVAideMemoire]{MVA.synt}}.

\code{fc.multcomp} is not useful anymore since \code{\link[lsmeans]{lstrends}} (package lsmeans) does the same job in a much more powerful manner (see argument \code{var} of \code{\link[lsmeans]{lstrends}}).

\code{friedman.rating.test}, \code{kruskal.rating.test}, \code{wilcox.rating.test}, \code{wilcox.rating.signtest}, \code{pairwise.wilcox.rating.test} and \code{wilcox.paired.rating.multcomp} can be problematic with ratings (in which ties and zeroes are very frequent). The use of CLM(M)s (\emph{via} \code{\link[ordinal]{clm}} and \code{\link[ordinal]{clmm}}) is recommended.

\code{pairwise.manova} is not useful anymore since \code{\link[lsmeans]{lsmeans}} (package lsmeans) does the same job in a much more powerful manner (on \code{"mlm"} objects, created by \code{\link{lm}} and not \code{\link{manova}})

\code{plot1comp.ind}, \code{plot1comp.var}, \code{s.corcircle2}, \code{scat.mix.categorical}, \code{scat.mix.numeric} and \code{scatter.coa2} are replaced by the more generic \code{\link[RVAideMemoire]{MVA.plot}}.

\code{PLSDA.ncomp} was not really useful and \code{\link[pls]{mvr}} does nearly the same job.

\code{PLSDA.test} is replaced by the more generic \code{\link[RVAideMemoire]{MVA.test}}.
}
