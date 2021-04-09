#' Combine draw, getweights, and boil
#'
#' This is exported, but don't call it directly.
#'
#' @param z a (conceptual) row of a data.frame given by \code{pick}.
#' @param m a model object.
#' @param x   matrix. a data set; a population.
#' @param f function. a boiler.
#' @param y  numeric. values of the dependent variable representing success under \code{f}.
#' @param B   matrix. A set of plausible coefficient vectors generated via \code{\link{consider}}.
#' @param w  numeric. weights with respect to which to take the \code{g} function. Default \code{1}.
#' @param g function. the desired one-number summary of the boiled values. Default \code{stats::\link[stats]{weighted.mean}}.
#' @param ... other parameters ignored for the moment, but to be implemented.
#' @return a \code{data.frame} of simulated outcome summary statistics.
#' @export
imagine <- function(z, m, x, f, y, B, w=1, g = stats::weighted.mean, ...) {
  # force correct data type
  e <- unique(as.character(z$term))
  E <- class(x[[e]])

  x[[e]] <- if(E %in% c("logical", "numeric", "integer")) { # in case we have a number, which may have been coerced to character
    methods::as(z$value, E)
  } else if(class(z$value) %in%  c("numeric", "integer")) { # class(z$value)=="logical" should be impossibru
    Q <- ifelse(E=="factor", levels, unique)(x[[e]])[z$value]    # in case we have coerced a factor to numeric

    if(E=="factor") {factor(Q, levels=levels(x[[e]]))} else {Q}
  } else {z$value}                                          # no? then we must have character or factor data as character

  A <- draw(m, x, B, ...)

  plyr::ldply(A[, -1, drop=FALSE], boil, f=f, y=y, w=w, g=g)
}
