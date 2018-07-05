#' @describeIn draw svymnlogit
#' @export
draw.svymnlogit <- function(m, x=stats::model.frame(m), n=1, ...) {
  if(!requireNamespace("svymnlogit")) {stop("You must install the svymnlogit package.")}

  f <- stats::formula(m)

  X <- stats::model.matrix(f, x)
  y <- all.vars(stats::formula(m))[1]
  Y <- ifelse(is.factor(x[[y]]), levels, unique)(x[[y]]) # dude

  b <- stats::coef(m)
  V <- stats::vcov(m)

  B <- mvtnorm::rmvt(n, V, stats::df.residual(m), as.numeric(b))
  colnames(B) <- rep(rownames(b), ncol(b))

  # inelegant use of anonymous function but we'll go with it for now
  B <- plyr::llply(plyr::llply(rownames(b), stringr::str_detect, string=colnames(B)), function(u, B) {B[, u]}, B = B)

  M <- plyr::llply(B, function(u) {as.matrix(X) %*% t(u)})

  names(M) <- rownames(b)

  M <- plyr::llply(M, reshape2::melt, varnames=c("id", "sim"), value.name="value")
  M <- plyr::ldply(M, .id="outcome")

  M$value <- exp(M$value)

  M <- reshape2::dcast(M, id + sim ~ outcome)

  M[[setdiff(Y, rownames(b))]] <- 1

  M$Y <- apply(M[, Y], 1, sample, x=Y, size=1, replace=FALSE)
  M$Y <- factor(M$Y, levels=Y)

  W <- reshape2::dcast(M, id ~ sim, value.var = "Y")

  W
}

#' @describeIn getweights svymnlogit
#' @export
getweights.svymnlogit <- function(m, ...) {stats::weights(m)}

#' @describeIn termz svymnlogit has no \code{terms} element
#' @export
termz.svymnlogit <- function(m) {stats::terms(stats::formula(m))}
