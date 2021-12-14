quasibinomial.QAIC <- function(link="logit") {
    res <- quasibinomial(link=link)
    res$aic <- binomial(link=link)$aic
    res
}
