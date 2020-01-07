back.lsmeans <- function(...) {
  stop("back.lsmeans() is deprecated, use back.emmeans() of this package (more generally, stop using package 'lsmeans' and change to package 'emmeans', its new version)")
}

byf.normhist <- function(...) {
  stop("byf.normhist() is deprecated, use byf.hist() of this package")
}

cor.sparse <- function(...) {
  stop("cor.sparse() is deprecated, use the more generic MVA.cor() of this package")
}

CvM.test <- function(...) {
  stop("CvM.test() is deprecated, use cramer.test() from package 'cramer' instead (additionally, CvM.test() did not perform a Cramer-von Mises test but an alternative Cramer test)")
}

DA.confusion <- function(...) {
  stop("DA.confusion() is deprecated, use the more generic MVA.cmv() or MVA.cv() of this package")
}

DA.valid <- function(...) {
  stop("DA.valid() is deprecated, use the more generic MVA.cmv() or MVA.cv() of this package")
}

DA.var <- function(...) {
  stop("DA.var() is deprecated, use the more generic MVA.synt() of this package")
}

dunn.test <- function(...) {
  stop("dunn.test() is deprecated, use dunnTest() from package 'FSA' instead")
}

fc.multcomp <- function(...) {
  stop("fc.multcomp() is deprecated, use the more powerful emtrends() (package 'emmeans') with the 'var' argument to compare slopes")
}

friedman.rating.test <- function(...) {
  stop("friedman.rating.test() is deprecated, use CLMs instead (package 'ordinal')")
}

kruskal.rating.test <- function(...) {
  stop("kruskal.rating.test() is deprecated, use CLMs instead (package 'ordinal')")
}

pairwise.manova <- function(...) {
  stop("pairwise.manova() is deprecated, use the more powerful emmeans() (package 'emmeans') on \"mlm\" objects")
}

pairwise.to.groups <- function(...) {
  stop("pairwise.to.groups() is deprecated")
}

pairwise.wilcox.rating.test <- function(...) {
  stop("paiwise.wilcox.rating.test() is deprecated, use CLMs instead (package 'ordinal')")
}

plot1comp.ind <- function(...) {
  stop("plot1comp.ind() is deprecated, use the more generic MVA.plot() of this package")
}

plot1comp.var <- function(...) {
  stop("plot1comp.var() is deprecated, use the more generic MVA.plot() of this package")
}

PLSDA.ncomp <- function(...) {
  stop("PLSDA.ncomp() is deprecated, use directly mvr() (package 'pls')")
}

PLSDA.test <- function(...) {
  stop("PLSDA.test() is deprecated, use the more generic MVA.test() of this package")
}

rating.lsmeans <- function(...) {
  stop("rating.lsmeans() is deprecated, use rating.emmeans() of this package (more generally, stop using package 'lsmeans' and change to package 'emmeans', its new version)")
}

s.corcircle2 <- function(...) {
  stop("s.corcircle2() is deprecated, use the more generic MVA.plot() of this package")
}

scat.mix.categorical <- function(...) {
  stop("scat.mix.categorical() is deprecated, use the more generic MVA.plot() of this package")
}

scat.mix.numeric <- function(...) {
  stop("scat.mix.numeric() is deprecated, use the more generic MVA.plot() of this package")
}

scatter.coa2 <- function(...) {
  stop("scatter.coa2() is deprecated, use the more generic MVA.plot() of this package")
}

wilcox.paired.rating.multcomp <- function(...) {
  stop("wilcox.paired.rating.multcomp() is deprecated, use CLMs instead (package 'ordinal')")
}

wilcox.rating.signtest <- function (...) {
  stop("wilcox.rating.signtest() is deprecated, use CLMs instead (package 'ordinal')")
}

wilcox.rating.test <- function(...) {
  stop("wilcox.rating.test() is deprecated, use CLMs instead (package 'ordinal')")
}

