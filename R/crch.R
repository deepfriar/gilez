#' @describeIn draw crch
#' @export
draw.crch <- function(m, x=stats::model.frame(m), n=1, ...) {
  K <- m$link$scale$name
  g <- if(K=="quadratic") {sqrt} else if(K=="log") {exp} else {function(x) {x}}

  X <- stats::model.matrix(stats::terms(m, "location"),  x)
  Z <- stats::model.matrix(stats::terms(m, "scale"),     x)

  colnames(Z) <- paste0("(scale)_", colnames(Z))

  b <- stats::coef(m)
  V <- stats::vcov(m)

  B <- mvtnorm::rmvnorm(n, b, V) # not actually rnorm, but can't figure out how to get df.residual for this model
  colnames(B) <- names(b)

  Q <- intersect(colnames(X), names(b))
  M <- as.matrix(X[, Q]) %*% t(B[, Q])
  M <- reshape2::melt(M, varnames=c("id",  "sim"), value.name="mean")

  R <- intersect(colnames(Z), names(b))
  P <- as.matrix(Z[, R]) %*% t(B[, R])
  P <- reshape2::melt(P, varnames=c("id",  "sim"), value.name="scale")

  M$scale <- g(P$scale)    # this feels unsafe but surely it is faster than a join

  M$Y <- stats::rnorm(nrow(M), M$mean, M$scale)

  M$Y[M$Y < M$cens$left]  <- M$cens$left
  M$Y[M$Y > M$cens$right] <- M$cens$right

  W <- reshape2::dcast(M, id ~ sim, value.var = "Y")

  W
}

#' @describeIn getweights crch
#' @inheritParams getweights.glm
#' @export
getweights.crch <- function(m, x, ...) {
  w <- stats::weights(m)

  if(is.null(w) | !identical(x, stats::model.frame(m))) {rep(1, nrow(x))} else {w}
}

