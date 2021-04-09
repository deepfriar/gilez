#' @describeIn draw betareg
#' @export
draw.betareg <- function(m, x=stats::model.frame(m), B, ...) {
  f <- m$link$mean$linkinv
  g <- m$link$precision$linkinv

  X <- stats::model.matrix(stats::terms(m, "mean"),      x)
  Z <- stats::model.matrix(stats::terms(m, "precision"), x)

  colnames(Z) <- paste0("(phi)_", colnames(Z))

  # version 0.2: B is now an argument
  # b <- stats::coef(m)
  # V <- stats::vcov(m)

  # B <- mvtnorm::rmvt(n, V, stats::df.residual(m), b)
  # colnames(B) <- names(b)

  Q <- intersect(colnames(X), colnames(B))
  M <- as.matrix(X[, Q, drop=FALSE]) %*% t(B[, Q, drop=FALSE])
  M <- reshape2::melt(M, varnames=c("id",  "sim"), value.name="mean")

  R <- intersect(colnames(Z), colnames(B))
  P <- as.matrix(Z[, R, drop=FALSE]) %*% t(B[, R, drop=FALSE])
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

