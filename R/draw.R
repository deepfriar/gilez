#' Draw \code{n} response vectors from a fitted model for a population
#' @param m the model.
#' @param x data.frame. The population of interest.
#' @param n numeric. Number of vectors to draw. Default \code{1}.
#' @param ... other arguments to \code{weights} or to methods.
#' @return a vector of responses.
#' @export
draw <- function(m, x, n=1, ...) {UseMethod("draw")}
