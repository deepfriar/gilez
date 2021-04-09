#' Draw \code{n} _parameter_ vectors from the fitted likelihood for a model
#' @param m the model.
#' @param x data.frame. The population of interest.
#' @param n numeric. Number of vectors to draw. Default \code{1}.
#' @param ... other arguments to particular methods, I guess
#' @return a matrix of parameter vectors.
#' @export
consider <- function(m, x, n=1, ...) {UseMethod("consider")}

#' @describeIn consider Default method for model objects not needing special handling.
#' @export
consider.default <- function(m, x, n=1, ...) {
  b <- stats::coef(m)
  V <- stats::vcov(m)

  B <- mvtnorm::rmvt(n, V, stats::df.residual(m), b)

  `colnames<-`(B, names(b))
}
