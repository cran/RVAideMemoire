\name{elogis}
\alias{elogis}
\title{
Empirical logistic transformation
}
\description{
Empirical logistic transformation for logistic models with data showing (quasi-)complete separation. The function is intended to be used as a link function in GLM(M)s.
}
\usage{
elogis()
}
\author{
Formula from McCullagh & Nelder in their seminal book 'Generalized Linear Models'. R code from Eric Wajnberg & Jean-Sebastien Pierre.
}
\examples{
# An example with 3 groups and complete separation (from E. Wajnberg)
tab <- data.frame(case=letters[1:3],yes=c(25,30,0),no=c(1,0,20))
tab
\dontrun{
mod <- glm(cbind(yes,no)~case,family=binomial(link=elogis()),data=tab)
# Warnings are normal
summary(mod)}
}