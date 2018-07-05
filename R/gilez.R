## TODO: pass ... to all the things

#' Substantive effects via simulation
#' @param m a model.
#' @param x a population within which to simulate effects. Default \code{model.frame(m)} (the original population).
#' @param Z a named ist of two or more contrasting values for each variable. Default \code{bestz(m, x)}.
#' @param f a function for boiling down results. This is a function, not a string. Default \code{value}.
#' @param y values of the outcome representing success under \code{f}. Default \code{0}.
#' @param n number of simulated response vectors to generate for each level for each test. Default \code{1000}.
#' @param parallel logical. Passed on to \code{\link[plyr]{ddply}} at the outermost (term and level) levels.
#' @param w weights with respect to which to take averages. Default \code{\link{getweights}(m, x)}.
#' @param ... other arguments to functions used within.
#' @return a tall \code{data.frame} with class \code{gilez} to pass to \code{\link{gdiff}} or \code{\link[wickr]{sumer}}.
#' @export
gilez <- function(m, x=stats::model.frame(m), Z=bestz(m, x), f=value, y=0, n=1000, parallel=FALSE, w=getweights(m, x), ...) {
  Y <- plyr::ddply(Z, c("term", "level"), imagine, m=m, x=x, f=f, y=y, n=n, .parallel=parallel, w=w)

  Y[, -ncol(Y)] <- lapply(Y[, -ncol(Y)], as.character)
  colnames(Y)[ncol(Y)] <- "value" # TODO: make not inelegant

  class(Y) <- c("gilez", class(Y))

  attributes(Y)$sumer <- attr(wickr::sumer(m), "sumer")

  Y
}

#' Contrast effects for different levels with each other
#' @param Y a \code{\link{gilez}} object
#' @return another \code{gilez} object but with more colummzz!!!1
#' @export
gdiff <- function(Y) {
  W <- Y

  colnames(W) <- stringr::str_replace(colnames(W), "level", "baseline")
  colnames(W) <- stringr::str_replace(colnames(W), "value", "minus")

  W <- plyr::join(Y, W, intersect(colnames(Y), colnames(W)))

  W <- W[W$level != W$baseline, ]

  W$value <- W$value - W$minus

  W <- W[, setdiff(colnames(W), "minus")]

  class(W) <- c("gilez", setdiff(class(W), "gilez"))

  attributes(W)$sumer <- attr(Y, "sumer")

  W
}
