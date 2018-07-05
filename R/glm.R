#' @describeIn draw glm
#' @export
draw.glm <- function(m, x=stats::model.frame(m), n=1, ...) {
  u <- stats::family(m)
  if(!u$family=="binomial") {stop("Only the binomial glm is supported so far.")}

  L <- u$link
  f <- if(L=="probit") {stats::pnorm} else if(L=="cauchit") {stats::pcauchy} else {stats::plogis} # !&#@ cloglog

  X <- stats::model.matrix(stats::formula(m), x) # [NdGTkiddingme.jpg]

  b <- stats::coef(m)

  V <- stats::vcov(m)

  B <- mvtnorm::rmvt(n, V, stats::df.residual(m), b)
  colnames(B) <- names(b)

  Q <- intersect(colnames(X), names(b))

  M <- as.matrix(X[, Q]) %*% t(B[, Q]) # why did this not fail... until now?!

  M <- reshape2::melt(M, varnames=c("id",  "sim"), value.name="mean")

  M$Y <- stats::rbinom(nrow(M), 1, f(M$mean))

  W <- reshape2::dcast(M, id ~ sim, value.var = "Y")

  W
}

#' @describeIn getweights glm
#' @param x the population
#' @export
getweights.glm <- function(m, x, ...) {
  w <- stats::weights(m) # TODO: what if we are using a subpopulation and the original weights are required oh noes

  if(is.null(w) | !identical(x, stats::model.frame(m))) {rep(1, nrow(x))} else {w}
}
