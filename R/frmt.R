###################################
### Output formatting functions ###
###################################

#' Results formatting for tables and reports
#'
#' These functions are simple wrappers around `paste()` and `nmbr()` to format
#'     results for printing.
#'
#' @param mean,sd,n,proportion,est,low,high Numeric vectors. If `proportion` is
#'     missing, it will be calculated as \code{n / sum(n)}.
#' @param accuracy,scale,prefix,suffix,percent,... Passed to `nmbr()` and
#'     `prct()`.
#' @param .sep Separator between lower and upper bounds of the CI; default is
#'     \code{' to '}.
#' @param format_paren Logical scalar. Should the values inside parentheses
#'     include the specified prefix and suffix formatters?
#'
#' @name output-formatting
NULL

#' @rdname output-formatting
#'
#' @export
mean_sd <- function(mean, sd, prefix = "", suffix = "", ...,
                    format_paren = TRUE) {
  if (!rlang::is_bool(format_paren)) stop_wrong_type("format_paren", "a logical scalar")
  else if (format_paren) {
    prefix_paren = prefix
    suffix_paren = suffix
  } else {
    prefix_paren = ""
    suffix_paren = ""
  }
  paste0(nmbr(mean, prefix = prefix, suffix = suffix, ...),
         ' (', nmbr(sd, prefix = prefix_paren, suffix = suffix_paren, ...), ')')
}

#' @rdname output-formatting
#'
#' @export
n_percent <- function(n, proportion, accuracy = 1, scale = 1, prefix = "", suffix = "",
                      percent = "%", ..., format_paren = TRUE) {
  if (!rlang::is_bool(format_paren)) stop_wrong_type("format_paren", "a logical scalar")
  else if (format_paren) {
    prefix_paren = prefix
    suffix_paren = percent
  } else {
    prefix_paren = ""
    suffix_paren = ""
  }
  if (missing(proportion)) proportion <- n / sum(n)
  paste0(nmbr(n, accuracy = 1, scale = scale, prefix = prefix, suffix = suffix, ...), ' (',
         prct(proportion, accuracy = accuracy, prefix = prefix_paren, percent = suffix_paren, ...),
         ')')
}

#' @rdname output-formatting
#'
#' @export
est_ci <- function(est, low, high, prefix = "", suffix = "", ...,
                   .sep = ' to ', format_paren = TRUE) {
  if (!rlang::is_string(.sep)) stop_wrong_type(".sep", "a string scalar")
  if (!rlang::is_bool(format_paren)) stop_wrong_type("format_paren", "a logical scalar")
  else if (format_paren) {
    prefix_paren = prefix
    suffix_paren = suffix
  } else {
    prefix_paren = ""
    suffix_paren = ""
  }

  paste0(nmbr(est, prefix = prefix, suffix = suffix, ...), ' (',
         nmbr(low, prefix = prefix_paren, suffix = suffix_paren, ...), .sep,
         nmbr(high, prefix = prefix_paren, suffix = suffix_paren, ...), ')')
}
