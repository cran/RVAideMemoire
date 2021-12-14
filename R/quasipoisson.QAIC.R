quasipoisson.QAIC <- function(link="log") {
    res <- quasipoisson(link=link)
    res$aic <- poisson(link=link)$aic
    res
}
