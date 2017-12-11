#' Get weights from a model object
#' @param m the model object.
#' @param ... other arguments to \code{weights} or to methods.
#' @return a vector of weights.
#' @export
getweights <- function(m, ...) {UseMethod("getweights")}

#' @describeIn getweights default method
#' @export
getweights.default <- function(m, ...) {stats::weights(m, ...)}
