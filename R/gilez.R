## TODO: pass ... to all the things
## TODO: totally rewrite in temrs of tidyverse functions

#' Substantive effects via simulation
#' @param m a model.
#' @param x a population within which to simulate effects. Default \code{model.frame(m)} (the original population).
#' @param Z a named ist of two or more contrasting values for each variable. Default \code{bestz(m, x)}.
#' @param f a function for boiling down results. This is a function, not a string. Default \code{value}.
#' @param y values of the outcome representing success under \code{f}. Default \code{0}.
#' @param n number of simulated response vectors to generate for each level for each test. Default \code{1000}.
#' @param parallel logical. Passed on to \code{\link[plyr]{ddply}} at the outermost (term and level) levels.
#' @param w weights with respect to which to take averages. Default \code{\link{getweights}(m, x)}.
#' @param g function. the desired one-number summary of the boiled values. Default \code{stats::\link[stats]{weighted.mean}}.
#' @param ... other arguments to functions used within.
#' @return a tall \code{data.frame} with class \code{gilez} to pass to \code{\link{gdiff}} or \code{\link[wickr]{sumer}}.
#' @export
gilez <- function(m, x=stats::model.frame(m), Z=bestz(m, x), f=value, y=0, n=1000, parallel=FALSE, w=getweights(m, x),
                  g = stats::weighted.mean, ...) {
  B <- consider(m, x, n, ...)

  Y <- plyr::ddply(Z, c("term", "level"), imagine, m=m, x=x, f=f, y=y, .parallel=parallel, w=w, B=B, g=g)

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

  W <- plyr::join(as.data.frame(Y), as.data.frame(W), intersect(colnames(Y), colnames(W)))

  W <- W[W$level != W$baseline, ]

  W$value <- W$value - W$minus

  W <- W[, setdiff(colnames(W), "minus")]

  class(W) <- c("gilez", setdiff(class(W), "gilez"))

  attributes(W)$sumer <- attr(Y, "sumer")

  W
}

#' Add simulated outputs across objects
#'
#' @param G a list of \code{\link{gilez}} objects
#' @return a \code{\link{gilez}} object
#' @export
add_up <- function(G) {
  H <- lapply(G, attr, which="sumer")
  H <- purrr::transpose(H)
  H <- lapply(H, unlist)
  H <- dplyr::as_tibble(H)
  H <- dplyr::group_by_if(H, function(x) {!is.numeric(x)})
  H <- dplyr::select(H, .data$n)
  H <- dplyr::summarise_all(H, sum)

  if(nrow(H) > 1) {stop("Foo! Don't add simulated outcomes across different kinds of models.")}

  G <- dplyr::tibble(obj = 1:length(G), gilez=G)
  G <- tidyr::unnest(G)
  G <- dplyr::group_by(G, .data$term, .data$level, .data$.id) # assume for now these are always there under these names? I think they are
  G <- dplyr::summarise(G, value = sum(.data$value), count = dplyr::n())

  if(length(table(G$count)) > 1) {stop("Foo! Don't add outcomes across simulations with different settings.")}

  G <- dplyr::select(G, -.data$count)

  class(G) <- c("gilez", setdiff(class(G), "gilez"))

  attributes(G)$sumer <- as.list(as.data.frame(H))

  G
}

#' @importFrom rlang .data
#' @export
rlang::.data
