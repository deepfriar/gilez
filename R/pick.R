#' Pick out best levels of a given X variable at which to view effects.
#' @param x a column from a population.
#' @return a tall data.frame with columns \code{level} and \code{value}.
#' @export
pick <- function(x) {UseMethod("pick")}

#' @describeIn pick return all levels of the factor
#' @export
pick.factor    <- function(x) {.makez(levels(x))}

#' @describeIn pick return FALSE and TRUE
#' @export
pick.logical   <- function(x) {.makez(c(FALSE, TRUE))}

#' @describeIn pick return all unique elements, sorted
#' @export
pick.character <- function(x) {.makez(sort(unique(x)))}

#' @describeIn pick return all unique elements, sorted
#' @export
pick.integer   <- function(x) {.makez(sort(unique(x)))}

#' @describeIn pick return the levels in the data and a one-unit increase
#' @export
pick.numeric   <- function(x) {
  i <- if(all(x >= 0 & x < 1)) {0.01} else {1} # crudely differentiate percentages from other numeric things

  j <- list() # because R CMD CHECK throws a fit about passing a variable named "" to list()

  j[[""]]            <- x
  j[[paste("+", i)]] <- x + i

  plyr::ldply(plyr::llply(j, reshape2::melt), .id="level")
}

.makez <- function(x) {data.frame(level=x, value=x)}
