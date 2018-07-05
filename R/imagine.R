#' Combine draw, getweights, and boil
#'
#' This is exported, but don't call it directly.
#'
#' @param z a (conceptual) row of a data.frame given by \code{pick}.
#' @param m a model.
#' @param x a data set; a population.
#' @param f a boiler.
#' @param y values of the dependent variable representing success under \code{f}.
#' @param n number of response vectors to simulate.
#' @param w weights with respect to which to take averages.
#' @return a \code{data.frame} of simulated outcome summary statistics.
#' @export
imagine <- function(z, m, x, f, y, n, w) {
  # force correct data type
  e <- unique(as.character(z$term))
  E <- class(x[[e]])

  x[[e]] <- if(E %in% c("logical", "numeric", "integer")) { # in case we have a number, which may have been coerced to character
    methods::as(z$value, E)
  } else if(class(z$value) %in%  c("numeric", "integer")) { # class(z$value)=="logical" should be impossibru
    Q <- ifelse(E=="factor", levels, unique)(x[[e]])[z$value]    # in case we have coerced a factor to numeric

    if(E=="factor") {factor(Q, levels=levels(x[[e]]))} else {Q}
  } else {z$value}                                          # no? then we must have character or factor data as character

  A <- draw(m, x, n)

  plyr::ldply(A[, -1, drop=FALSE], boil, f=f, y=y, w=w)
}
