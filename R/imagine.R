#' Combine draw, getweights, and boil
#'
#' This is exported, but don't call it directly.
#'
#' @param z a row of a data.frame given by \code{pick}.
#' @param m a model.
#' @param x a data set; a population.
#' @param f a boiler.
#' @param y values of the dependent variable representing success under \code{f}.
#' @param n number of response vectors to simulate.
#' @return a \code{data.frame} of simulated outcome summary statistics.
#' @export
imagine <- function(z, m, x, f, y, n) {
  x[[z$term]] <- z$level

  A <- draw(m, x, n)

  plyr::ldply(A[, -1, drop=FALSE], boil, f=f, y=y, w=getweights(m, x))
}
