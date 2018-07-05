#' @describeIn draw the \code{x} argument may not be omitted
#' @export
draw.svyolr <- function(m, x, n=1, ...) {
  if(!requireNamespace("survey")) {stop("You must install the survey package.")}

  L <- m$method
  f <- if(L=="probit") {stats::pnorm} else if(L=="cauchit") {stats::pcauchy} else {stats::plogis} # !&#@ cloglog

  X <- stats::model.matrix(m, x)

  b <- stats::coef(m)
  V <- stats::vcov(m)

  B <- mvtnorm::rmvt(n, V, stats::df.residual(m), b)
  colnames(B) <- names(b)

  Q <- intersect(colnames(X), names(b))

  M <- as.matrix(X[, Q]) %*% t(B[, Q, drop=FALSE]) # why did this not fail... until now?!

  z <- sort(b[setdiff(names(b), Q)]) # sort() here is defensive programming. I don't know if it will ever matter
  Z <- B[, names(z), drop=FALSE]

  Z <- reshape2::melt(Z, varnames=c("sim", "level"), value.name="thresh")
  M <- reshape2::melt(M, varnames=c("id",  "sim"),   value.name="mean")

  R <- plyr::join(M, Z, "sim")

  R$p    <- f(R$mean - R$thresh, lower.tail = FALSE)
  R$level <- factor(R$level, levels=names(z))

  S <- reshape2::dcast(R, id + sim ~ level, value.var = "p")
  U <- apply(data.frame(floor=0, S[, names(z)], roof=1), 1, diff)

  S$Y <- apply(U, 2, sample, x=m$lev, size=1, replace=FALSE)
  S$Y <- as.numeric(factor(S$Y, levels=m$lev))

  W <- reshape2::dcast(S, id ~ sim, value.var = "Y")

  W
}

#' @describeIn getweights svyolr
#' @inheritParams getweights.glm
#' @export
getweights.svyolr <- function(m, x, ...) {stats::model.frame(m, x)$`(weights)`}
