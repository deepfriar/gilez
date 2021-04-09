#' Draw \code{n} response vectors from a fitted model for a population
#' @param m the model.
#' @param x data.frame. The population of interest.
#' @param B matrix. A set of plausible coefficient vectors generated via \code{\link{consider}}.
#' @param ... other arguments to \code{weights} or to methods.
#' @return a vector of responses.
#' @export
draw <- function(m, x, B, ...) {UseMethod("draw")}
