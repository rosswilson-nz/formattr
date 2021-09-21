#' Print 'significance' stars for given P-value statistics
#'
#' Print significance stars representing p < 0.01, p < 0.05, p < 0.1 or custom
#'     significance levels. The usual caveats about the misuse of p-values
#'     apply - these should not be taken to represent the correctness or
#'     otherwise of a given alternative hypothesis.
#'
#' @param p Vector of p-values.
#' @param levels Named vector of significance levels. The values should be the
#'     upper threshold of each level, and the names the associated labels to be
#'     printed.
#'
#' @export
sstars <- function(p, levels = c("*" = 0.1, "**" = 0.05, "***" = 0.01)) {
  if (!rlang::is_bare_numeric(p)) stop_wrong_type("p", "a numeric vector")
  if (!rlang::is_bare_numeric(levels) || !rlang::is_named(levels))
    stop_wrong_type("levels", "a named numeric vector")

  thresholds <- c(sort(levels), Inf)
  idx <- findInterval(p, thresholds) + 1
  names(thresholds)[idx]
}
