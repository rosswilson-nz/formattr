###################################################################
### Number formatting functions (based on scales::number() etc) ###
###################################################################

#' Format numbers using `scales::number()`-type functions
#'
#' These functions extend the \code{\link[scales]{number}}-type formatting
#'     functions, with nice printing of negative numbers, optional replacement
#'     of missing values, and vectorised formatting options.
#'
#' @param x Numeric vector to format
#' @param accuracy,scale,prefix,suffix,big.mark,decimal.mark,... As in
#'     \code{\link[scales]{number}}. If a vector is supplied, will be applied
#'     element-wise to `x` (and must have the same length as `x`).
#' @param html Logical scalar. Whether to include formatting marks (minus
#'     signs and narrow spaces between digits) as HTML strings (the default;
#'     best for Word or HTML output documents) or unicode.
#' @param na String scalar, replacement to use for missing values in `x`.
#' @param percent,comma,dollar String scalar to use for the specific formatting
#'     method (percent sign, comma separator, dollar sign, etc).
#' @param min_p Numeric scalar. The smallest p-value to print; values smaller
#'     than this will be printed as \code{"<`min_p`".}
#' @param add_p Logical scalar. Should `p=` be included before formatted
#'     p-values?
#'
#' @return For `create_nmbr()`, a function with the same arguments as `nmbr()`;
#'     for other functions a character string applying the specified formatting
#'     rules to `x`.
#'
#' @name number-formatting
NULL

#' @rdname number-formatting
#' @export
nmbr <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "",
                 big.mark = "< >", decimal.mark = ".", html = TRUE, na = NA_character_,
                 ...) {
  if (length(x) == 0) return(character())
  args <- list(x = x, accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, html = html, na = na)
  check_nmbr_args(args)

  x <- round(x*scale/accuracy) * accuracy/scale

  minus <- if (html) "&minus;" else "\u2212"
  neg <- rep("", length(x))
  neg[x < 0] <- minus

  narrow_space <- if (html) "&#x202F;" else "\u202F"

  if (any(lengths(args[-1]) > 1)) {
    accuracy <- rlang::rep_along(x, accuracy)
    scale <- rlang::rep_along(x, scale)
    big.mark <- rlang::rep_along(x, big.mark)
    decimal.mark <- rlang::rep_along(x, decimal.mark)

    nsmall <- pmin(pmax(-floor(log10(accuracy)), 0), 20)

    frmt <- vapply(
      seq_along(x),
      function(i) format(abs(scale[i] * x[i]), big.mark = big.mark[i],
                         decimal.mark = decimal.mark[i], trim = TRUE, nsmall = nsmall[i],
                         scientific = FALSE, ...),
      character(1)
    )
  } else {
    nsmall <- min(max(-floor(log10(accuracy)), 0), 20)

    frmt <- format(abs(scale * x), big.mark = big.mark, decimal.mark = decimal.mark, trim = TRUE,
                   nsmall = nsmall, scientific = FALSE, ...)
  }

  ret <- stringi::stri_replace_all_regex(paste0(neg, prefix, frmt, suffix), "< >", narrow_space)
  ret[is.na(x)] <- na
  names(ret) <- names(x)
  ret
}

check_nmbr_args <- function(args) {
  if (!is.null(args$x)) {
    if (!rlang::is_bare_numeric(eval(args$x))) stop_wrong_type("x", "a numeric vector")
  }

  if (!is.null(args$accuracy) && !rlang::is_bare_numeric(args$accuracy))
    stop_wrong_type("accuracy", "a numeric vector/scalar")
  if (!is.null(args$scale) && !rlang::is_bare_numeric(args$scale))
    stop_wrong_type("scale", "a numeric vector/scalar")
  if (!is.null(args$prefix) && !rlang::is_bare_character(args$prefix))
    stop_wrong_type("prefix", "a character vector/scalar")
  if (!is.null(args$suffix) && !rlang::is_bare_character(args$suffix))
    stop_wrong_type("suffix", "a character vector/scalar")
  if (!is.null(args$big.mark) && !rlang::is_bare_character(args$big.mark))
    stop_wrong_type("big.mark", "a character vector/scalar")
  if (!is.null(args$decimal.mark) && !rlang::is_bare_character(args$decimal.mark))
    stop_wrong_type("decimal.mark", "a character vector/scalar")

  if (!is.null(args$html) && !rlang::is_bool(args$html))
    stop_wrong_type("html", "`TRUE`/`FALSE`")
  if (!is.null(args$na) && !rlang::is_scalar_character(args$na))
    stop_wrong_type("na", "a string scalar")

  if (!is.null(args$x)) {
    lenx <- length(args$x)
    if (!is.null(args$accuracy) && length(args$accuracy) != 1 && length(args$accuracy) != lenx)
      stop_wrong_length("accuracy", lenx, length(args$accuracy))
    if (!is.null(args$scale) && length(args$scale) != 1 && length(args$scale) != lenx)
      stop_wrong_length("scale", lenx, length(args$scale))
    if (!is.null(args$prefix) && length(args$prefix) != 1 && length(args$prefix) != lenx)
      stop_wrong_length("prefix", lenx, length(args$prefix))
    if (!is.null(args$suffix) && length(args$suffix) != 1 && length(args$suffix) != lenx)
      stop_wrong_length("suffix", lenx, length(args$suffix))
    if (!is.null(args$big.mark) && length(args$big.mark) != 1 && length(args$big.mark) != lenx)
      stop_wrong_length("big.mark", lenx, length(args$big.mark))
    if (!is.null(args$decimal.mark) && length(args$decimal.mark) != 1
        && length(args$decimal.mark) != lenx)
      stop_wrong_length("decimal.mark", lenx, length(args$decimal.mark))
  }
}

#' @rdname number-formatting
#' @export
prct <- function(x, percent = "%", accuracy = 1, prefix = "", big.mark = "< >", decimal.mark = ".",
                 html = TRUE, na = NA_character_, ...) {
  nmbr(x, accuracy = accuracy, scale = 100, prefix = prefix, suffix = percent, big.mark = big.mark,
       decimal.mark = decimal.mark, html = html, na = na, ...)
}

#' @rdname number-formatting
#' @export
cmma <- function(x, comma = ",", accuracy = 1, scale = 1, prefix = "", suffix = "",
                 decimal.mark = ".", html = TRUE, na = NA_character_, ...) {
  nmbr(x, accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix, big.mark = comma,
       decimal.mark = decimal.mark, html = html, na = na, ...)
}

#' @rdname number-formatting
#' @export
dllr <- function(x, dollar = "$", accuracy = 1, scale = 1, suffix = "", big.mark = "< >",
                 decimal.mark = ".", html = TRUE, na = NA_character_, ...) {
  nmbr(x, accuracy = accuracy, scale = scale, prefix = dollar, suffix = suffix, big.mark = big.mark,
       decimal.mark = decimal.mark, html = html, na = na, ...)
}

#' @rdname number-formatting
#' @export
pval <- function(x, accuracy = 0.0001, min_p = accuracy, add_p = FALSE,
                 decimal.mark = ".", na = NA_character_, ...) {
  if (!rlang::is_bare_numeric(accuracy, 1)) stop_wrong_type("accuracy", "a numeric scalar")
  if (!rlang::is_bare_numeric(min_p, 1)) stop_wrong_type("min_p", "a numeric scalar")
  if (min_p < accuracy) stop_invalid_min_p(accuracy, min_p)
  if (!rlang::is_bool(add_p)) stop_wrong_type("add_p", "a logical scalar")

  out <- nmbr(x, accuracy = accuracy, decimal.mark = decimal.mark)
  out[x < min_p] <- paste0("<", nmbr(min_p, accuracy = min_p, decimal.mark = decimal.mark))

  if (add_p) {
    out[x < min_p] <- paste0("p", out[x < min_p])
    out[x >= min_p] <- paste0("p=", out[x >= min_p])
  }

  out
}

#' @rdname number-formatting
#' @export
create_nmbr <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = "< >",
                        decimal.mark = ".", html = TRUE, na = NA_character_, ...) {
  args <- list(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, html = html, na = na)
  check_nmbr_args(args)

  function(x) nmbr(x, accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
                   big.mark = big.mark, decimal.mark = decimal.mark, html = html, na = na, ...)
}
