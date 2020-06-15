elogis <- function () {
  linkfun <- function(mu) log((mu+0.5)/(1-mu+0.5))
  linkinv <- function(eta) ((1+0.5)*exp(eta)-0.5)/(1+exp(eta))
  mu.eta <- function(eta) (1+1)/(exp(-eta)+exp(eta)+2)
  valideta <- function(eta) TRUE
  link <- "elogis"
  structure(list(linkfun=linkfun,linkinv=linkinv,mu.eta=mu.eta,
    valideta=valideta,name=link),class="link-glm")
}
