#' Weighted sum g-function
#' Just \code{sum(x * w)} in a nice wrapper
#' @param x boiler output
#' @param w "weights" (which should really be counts). Default \code{1}.
#' @param ... ignored.
#' @return \code{sum(x * w)}
#' @export
weighted.sum <- function(x, w=1, ...) {sum(x * w)}
