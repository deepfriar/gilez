#' Take the weighted mean of a function of a vector
#' @param x the input.
#' @param f a \code{function}, typically to convert \code{x} to \code{logical}. Default \code{\link{value}} (atypical).
#' @param g a \code{function} to summarize the output. Default \code{\link[stats]{weighted.mean}}.
#' @param w weights. Default \code{1}.
#' @param ... other arguments to \code{f}. Most cases take an argument \code{y} of values corresponding to success.
#' @return a data.frame with a numeric value for each possible outcome.
#' @export
boil <- function(x, f=value, g=stats::weighted.mean, w=rep(1, length(x)), ...) {
  z <- f(x, ...)

  z <- if(methods::is(z, "list")) {z} else {list(z)}

  plyr::ldply(z, g, w=w, .id="outcome")
}
