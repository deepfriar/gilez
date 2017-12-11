#' Output your input (the trivial boiler)
#' @param x the output.
#' @param ... ignored.
#' @return the input.
#' @export
value <- function(x, ...) {x}

#' A wrapper for \code{\%in\%}
#' @param x the input.
#' @param y value(s) of \code{x} corresponding to success. Default \code{0}.
#' @param ... ignored.
#' @return an object the same shape as \code{x} and \code{mode} \code{logical}.
#' @export
among <- function(x, y=0, ...) {x %in% y}

#' A wrapper for \code{among}, confusingly
#' @param x the input.
#' @param y value(s) of \code{x} corresponding to success. Default \code{unique(x)}.
#' @param ... ignored.
#' @return a list of results for \code{among}, one for each separate value in \code{y}
#' @export
equal <- function(x, y=unique(x), ...) {
  y <- if(all(is.atomic(y))) {sort(y)} else {y}

  z <- lapply(y, among, x=x)

  names(z) <- as.character(y)

  z
}

#' A wrapper for \code{>}
#' @inherit among params return
#' @export
above <- function(x, y, ...) {as.numeric(x) > y}

#' A wrapper for \code{<}
#' @inherit among params return
#' @export
below <- function(x, y, ...) {as.numeric(x) < y}

