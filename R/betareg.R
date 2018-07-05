#' @describeIn draw betareg
#' @export
draw.betareg <- function(m, x=stats::model.frame(m), n=1, ...) {
  L <- m$link$mean$name
  f <- if(L=="probit") {stats::pnorm} else if(L=="cauchit") {stats::pcauchy} else {stats::plogis} # !&#@ cloglog

  K <- m$link$precision$name
  g <- if(K=="sqrt") {sqrt} else if(K=="log") {log} else {function(x) {x}}

  X <- stats::model.matrix(stats::terms(m, "mean"),      x)
  Z <- stats::model.matrix(stats::terms(m, "precision"), x)

  colnames(Z) <- paste0("(phi)_", colnames(Z))

  b <- stats::coef(m)
  V <- stats::vcov(m)

  B <- mvtnorm::rmvt(n, V, stats::df.residual(m), b)
  colnames(B) <- names(b)

  Q <- intersect(colnames(X), names(b))
  M <- as.matrix(X[, Q]) %*% t(B[, Q])
  M <- reshape2::melt(M, varnames=c("id",  "sim"), value.name="mean")

  R <- intersect(colnames(Z), names(b))
  P <- as.matrix(Z[, R]) %*% t(B[, R])
  P <- reshape2::melt(P, varnames=c("id",  "sim"), value.name="precision")

  M$phi   <- g(P$precision)    # this feels unsafe but surely it is faster than a join
  M$alpha <- f(M$mean) * M$phi

  M$Y <- stats::rbeta(nrow(M), M$alpha, M$phi - M$alpha)

  W <- reshape2::dcast(M, id ~ sim, value.var = "Y")

  W
}

#' @describeIn getweights betareg
#' @inheritParams getweights.glm
#' @export
getweights.betareg <- function(m, x, ...) {
  w <- stats::weights(m)

  if(is.null(w) | !identical(x, stats::model.frame(m))) {rep(1, nrow(x))} else {w}
}

