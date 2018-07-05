#' Wrapper for best way to pull out model terms
#'
#' Write a method for your model \code{m} if \code{stats::\link[stats]{terms}(m)} does not work
#'
#' @param m the model.
#' @return a \code{terms} object
#' @export
termz <- function(m) {UseMethod("termz")}

#' @describeIn termz For most models, just a wrapper for \code{stats::terms}
#' @export
termz.default <- function(m) {stats::terms(m)}
