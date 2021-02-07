#' @useDynLib miscSmooth
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
#' ysr <- local_smoothR(x, y, 0.55, xo)
#' all.equal(ys, ysr)
#' 
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

#' Poor person's density
#'
#' @param x numeric vector of observations
#' @param scale bandwidth for a Gaussian kernel
#' @param xo numeric vector of points where kde is evaluated
#'
#' @return An xy-list of class "density"
#' @export
#'
#' @examples
#' set.seed(1234)
#' x <- rnorm(5000)
#' dx <- density_x(x)
#' ox <- density(x)
#' par(mfcol = c(1,2), las = 1)
#' plot(dx, panel.first = grid(), col = 2)
#' plot(ox, panel.first = grid(), col = 4)
density_x <- function(x, scale = bw.nrd0(x),
                      xo = seq(min(x) - 3*scale, max(x) + 3*scale, length = 512)) {
  y <- densityCpp(x, scale, xo)
  # y <- y/sum((y[-1] + y[-length(y)])*diff(xo)/2)
  d <- list(x = xo, y = y, bw = scale, n = length(x),
            call = match.call(), data.name = deparse(substitute(x)),
            has.na = FALSE)
  class(d) <- "density"
  d
}
