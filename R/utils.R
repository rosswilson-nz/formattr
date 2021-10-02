is_all_character <- function(x) all(vapply(x, rlang::is_bare_character, logical(1)))

has_names <- function(x) {
  nms <- names(x)
  if (rlang::is_null(nms)) return(FALSE)
  if (all(nms == "" | is.na(nms))) return(FALSE)
  TRUE
}

lq <- function(x, ...) unname(stats::quantile(x, probs = 0.25, ...))
uq <- function(x, ...) unname(stats::quantile(x, probs = 0.75, ...))

`%||%` <- function(x, y) if (is.null(x)) y else x
