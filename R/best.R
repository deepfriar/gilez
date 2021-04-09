#' Pick out best levels of each X variable at which to view effects.
#'
#' This function kind of assumes the goal is \code{\link{gdiff}}.
#'
#' @param m a model.
#' @param x a population.
#' @return a tall data.frame with columns \code{term}, \code{level}, and \code{value}.
#' @export
bestz <- function(m, x) {
  labs <- attr(termz(m), "term.labels")

  labs <- intersect(labs, colnames(x))

  names(labs) <- labs # needed?

  plyr::ldply(x[, labs, drop=FALSE], pick, .id="term")
}
