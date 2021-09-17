stop_wrong_type <- function(arg, required) {
  x <- paste0(
    "Invalid argument.\n",
    rlang::format_error_bullets(c(i = paste0("`", arg, "` must be ", required, ".")))
  )
  rlang::cnd_signal(rlang::error_cnd("formattr_error_wrong_type", message = x))
}

stop_wrong_length <- function(arg, lenx, lenarg) {
  x <- paste0(
    "Invalid argument.\n",
    rlang::format_error_bullets(c(
      i = paste0("`", arg, "` must have be either a scalar or have the same length as `x`"),
      x = paste0("`", arg, "` has length ", lenarg, ", but `x` has length ", lenx, ".")
    ))
  )
  rlang::cnd_signal(rlang::error_cnd("formattr_error_wrong_type", message = x))
}
