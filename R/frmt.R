###################################
### Output formatting functions ###
###################################

#' Results formatting for tables and reports
#'
#' These functions are simple wrappers around \code{\link[base]{paste}} and
#'     \code{\link{nmbr}} to format results for printing.
#'
#' @param mean,sd,n,proportion,est,low,high,coef,se,p Numeric vectors. If
#'     `proportion` is missing, it will be calculated as \code{n / sum(n)}.
#'     If `p` is provided for `est_ci()` and `coef_se()`, p-values (if
#'     \code{stars = FALSE}) or significance stars (if \code{stars = TRUE})
#'     will be printed after the coefficient and standard error/confidence
#'     interval.
#' @param accuracy,scale,prefix,suffix,percent,... Passed to \code{\link{nmbr}}
#'     and \code{\link{prct}}.
#' @param .sep Separator between lower and upper bounds of the CI; default is
#'     \code{' to '}.
#' @param format_paren Logical scalar. Should the values inside parentheses
#'     include the specified prefix and suffix formatters?
#' @param stars Logical scalar. Should significance stars (`TRUE`) or p-values
#'     (`FALSE`) be printed after confidence intervals or standard errors (only
#'     used when `p` is provided).
#' @param levels Passed to \code{\link{stars}}.
#' @param accuracy_p,min_p,add_p Passed to \code{\link{pval}} (`accuracy_p` as
#'     the `accuracy` argument of `pval()`).
#'
#' @name output-formatting
NULL

#' @rdname output-formatting
#'
#' @export
mean_sd <- function(
  mean,
  sd,
  prefix = "",
  suffix = "",
  ...,
  format_paren = TRUE
) {
  if (!rlang::is_bool(format_paren)) {
    stop_wrong_type("format_paren", "a logical scalar")
  } else if (format_paren) {
    prefix_paren = prefix
    suffix_paren = suffix
  } else {
    prefix_paren = ""
    suffix_paren = ""
  }
  paste0(
    nmbr(mean, prefix = prefix, suffix = suffix, ...),
    ' (',
    nmbr(sd, prefix = prefix_paren, suffix = suffix_paren, ...),
    ')'
  )
}

#' @rdname output-formatting
#'
#' @export
n_percent <- function(
  n,
  proportion,
  accuracy = 1,
  scale = 1,
  prefix = "",
  suffix = "",
  percent = "%",
  ...,
  format_paren = TRUE
) {
  if (!rlang::is_bool(format_paren)) {
    stop_wrong_type("format_paren", "a logical scalar")
  } else if (format_paren) {
    prefix_paren = prefix
    suffix_paren = percent
  } else {
    prefix_paren = ""
    suffix_paren = ""
  }
  if (missing(proportion)) {
    proportion <- n / sum(n)
  }
  paste0(
    nmbr(n, accuracy = 1, scale = scale, prefix = prefix, suffix = suffix, ...),
    ' (',
    prct(
      proportion,
      accuracy = accuracy,
      prefix = prefix_paren,
      percent = suffix_paren,
      ...
    ),
    ')'
  )
}

#' @rdname output-formatting
#'
#' @export
est_ci <- function(
  est,
  low,
  high,
  p,
  prefix = "",
  suffix = "",
  ...,
  .sep = ' to ',
  format_paren = TRUE,
  stars = TRUE,
  levels = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  accuracy_p = 0.0001,
  min_p = accuracy_p,
  add_p = TRUE
) {
  if (!rlang::is_string(.sep)) {
    stop_wrong_type(".sep", "a string scalar")
  }
  if (!rlang::is_bool(stars)) {
    stop_wrong_type("stars", "a logical scalar")
  }
  if (!rlang::is_bool(format_paren)) {
    stop_wrong_type("format_paren", "a logical scalar")
  } else if (format_paren) {
    prefix_paren = prefix
    suffix_paren = suffix
  } else {
    prefix_paren = ""
    suffix_paren = ""
  }
  sig <- if (!missing(p)) {
    if (stars) {
      ifelse(is.na(p), "", paste0(" ", sstars(p, levels = levels)))
    } else {
      ifelse(
        is.na(p),
        "",
        paste0(
          " (",
          pval(p, accuracy = accuracy_p, min_p = min_p, add_p = add_p),
          ")"
        )
      )
    }
  }

  ci <- ifelse(
    is.na(low) | is.na(high),
    "",
    paste0(
      ' (',
      nmbr(low, prefix = prefix_paren, suffix = suffix_paren, ...),
      .sep,
      nmbr(high, prefix = prefix_paren, suffix = suffix_paren, ...),
      ')'
    )
  )

  paste0(nmbr(est, prefix = prefix, suffix = suffix, ...), ci, sig)
}

#' @rdname output-formatting
#'
#' @export
coef_se <- function(
  coef,
  se,
  p,
  prefix = "",
  suffix = "",
  ...,
  stars = TRUE,
  levels = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  accuracy_p = 0.0001,
  min_p = accuracy_p,
  add_p = TRUE
) {
  if (!rlang::is_bool(stars)) {
    stop_wrong_type("stars", "a logical scalar")
  }

  se <- if (missing(se)) {
    ""
  } else {
    ifelse(is.na(se), "", paste0(' (', nmbr(se, ...), ')'))
  }

  sig <- if (!missing(p)) {
    if (stars) {
      ifelse(is.na(p), "", paste0(" ", sstars(p, levels = levels)))
    } else {
      ifelse(
        is.na(p),
        "",
        paste0(
          " (",
          pval(p, accuracy = accuracy_p, min_p = min_p, add_p = add_p),
          ")"
        )
      )
    }
  }

  paste0(nmbr(coef, prefix = prefix, suffix = suffix, ...), se, sig)
}
