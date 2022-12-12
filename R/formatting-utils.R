#' Specify significant figures for printing
#'
#' This function specifies the number of significant figures to print (when\
#'     passed to the \code{accuracy} argument of \code{\link{nmbr}()} and
#'     related functions).
#'
#' @param x Vector of values for which significant figures are calculated
#' @param n Number of significant figures to print
#' @param dp_min (optional) Minimum number of decimal places to print. Use
#'     negative values for positive powers of 10; e.g., \code{dp_min = -2} will
#'     print all values to at least the 100's digit, even if that results in
#'     more than \code{n} significant figures.
#' @param dp_max (optional) Maximum number of decimal places to print. Use
#'     negative values for positive powers of 10; e.g., \code{dp_max = -2} will
#'     print all values to at most the 100's digit, even if that results in
#'     fewer than \code{n} significant figures.
#'
#' @return A vector of the same length as \code{x}, suitable to be passed to the
#'     \code{accuracy} argument of \code{\link{nmbr}()} and related functions.
#'
#' @seealso \code{\link{dp}}, \code{\link{number-formatting}}
#'
#' @export
sf <- function(x, n, dp_min = -Inf, dp_max = Inf) {
  if (any(n < 1)) stop("`n` must be at least 1")
  dp <- floor(log10(abs(x))) - (n - 1)
  dp[dp > -dp_min] <- -dp_min
  dp[dp < -dp_max] <- -dp_max
  10 ^ dp
}

#' Specify decimal places for printing
#'
#' This function specifies the number of decimal places to print (when\
#'     passed to the \code{accuracy} argument of \code{\link{nmbr}()} and
#'     related functions).
#'
#' @param n Number of decimal places to print. Use negative values for positive
#'     powers of 10; e.g. \code{n = -2} will print values rounded to the nearest
#'     multiple of 100.
#'
#' @return A vector of the same length as \code{n}, suitable to be passed to the
#'     \code{accuracy} argument of \code{\link{nmbr}()} and related functions.
#'
#' @seealso \code{\link{sf}}, \code{\link{number-formatting}}
#'
#' @export
dp <- function(n) 10 ^ -n
