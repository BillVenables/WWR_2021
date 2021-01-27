#' @useDynLib pirate
#' @import Rcpp
#' @import stats
NULL

#' Local exponential smoothing
#'
#' A Local average mean estimate.
#'
#'
#' @param x,y  numeric vectors with x- and y-values respectively
#' @param scale A positive scalar determining the rate of decay of the weights
#' @param xo numeric vector at which the smoothed estimate is to be evaluated
#'
#' @return A numeric vector of smoothed y-values
#' @export
#'
#' @examples
#' set.seed(1234)
#' x <- sort(rnorm(500))
#' y <- x + rnorm(x, 0, 2)
#' xo <- seq(min(x), max(x), len = 250)
#' ys <- local_smooth(x, y, 0.55, xo)
#' plot(x, y, pch = ".", cex = 2,
#'      las = 1, panel.first = grid())
#' lines(xo, ys, col = 2)
#' lines(lowess(x, y), col=4)
local_smooth <- function(x, y, scale, xo = x) {
  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y), length(x) > 0,
            is.numeric(scale), length(scale) == 1, scale > 0,
            is.numeric(xo))
  return(localWeightCpp(x, y, scale, xo))
}

#' @rdname local_smooth
#' @export
local_smoothR <- function(x, y, scale, xo = x) {
  stopifnot(is.numeric(x), is.numeric(y), length(x) == length(y), length(x) > 0,
            is.numeric(scale), length(scale) == 1, scale > 0,
            is.numeric(xo))
  ys <- numeric(length(xo))
  for(i in seq_along(xo)) {
    ys[i] <- weighted.mean(y, w = dnorm(x, xo[i], scale))
  }
  return(ys)
}
