## TODO: pass ... to all the things

#' Substantive effects via simulation
#' @param m a model.
#' @param x a population within which to simulate effects. Default \code{model.frame(m)} (the original population).
#' @param Z a named ist of two or more contrasting values for each variable. Default \code{pick(m)}.
#' @param f a function for boiling down results. This is a function, not a string. Default \code{value}.
#' @param y values of the outcome representing success under \code{f}. Default \code{0}.
#' @param n number of simulated response vectors to generate for each level for each test. Default \code{1000}.
#' @param ... other arguments to functions used within.
#' @return a \code{sumer} object (a tidy summary, with model metadata as an attribute). (Well, eventually.)
#' @export
gilez <- function(m, x=stats::model.frame(m), Z=pick(m), f=value, y=0, n=1000, ...) {
  x <- stats::model.frame(m, x)

  ## TODO: what if we just want to simulate coefficients, not results? or (gasp) expected values?
  ## well, OK, expected values is just a boiler or similar  ## no it's not
  Y <- plyr::ddply(Z, c("term", "level"), imagine, m=m, x=x, f=f, y=y, n=n)

  Y[, -ncol(Y)] <- lapply(Y[, -ncol(Y)], as.character)
  colnames(Y)[ncol(Y)] <- "value" # TODO: make not inelegant

  class(Y) <- c("gilez", class(Y))

  attributes(W)$sumer <- attr(wickr::sumer(m), "sumer")

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

#' Functional stub for smart contrasting-value function
#' @param m a model
#' @return for now, a list of numeric vectors, each with value \code{c(0, 1)}, one for each predictor in the model
#' @export
pick <- function(m) {
  foo <- attr(stats::terms(m), "term.labels")

  names(foo) <- foo

  # TODO: c(0, 1) here to be replaced with sensible function
  bar <- plyr::ldply(foo, function(x) {c(0, 1)}, .id="term")

  reshape2::melt(bar, value.name="level")[, c("term", "level")]
}
