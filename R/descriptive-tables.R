###################################################################################################
### Functions to create formatted descriptive tables for both continuous and discrete variables ###
###################################################################################################

if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".mean", ".sd", ".n", ".proportion", "Patient characteristic"))

#' Format descriptive statistics tables
#'
#' Construct nicely formatted descriptive statistics tables (mean/sd, n/percent).
#'
#' @param df A data frame containing the variables to describe.
#' @param continuous,discrete Character vectors of continuous and discrete
#'     variables for which to calculate descriptive statistics. Can be named,
#'     in which case the provided names will be used in place of variable names
#'     in the first column of the table.
#' @param multiresponse List of character vectors, each specifying the
#'     individual variables making up a combined multiple-response variable.
#'     Should be named, with the names giving the variable names to be used
#'     for the combined variables.
#' @param output (optional) Character vector of variables to include in the
#'     table (and their order). Elements should be the names of elements of
#'     `continuous`, `discrete`, and `multireponse`. Default is to report all
#'     variables from `continuous`, `discrete`, and `multiresponse`, in that
#'     order.
#' @param by (optional) A grouping variable (as a character string) to split
#'     the sample. Default is to report for the entire sample.
#' @param value_continuous (optional) An expression describing how to format
#'     the outcome values for continuous variables. Can refer to
#'     mean/sd/min/lq/median/uq/max as `.mean`, `.sd`, etc, and the name of the
#'     variable being formatted as `.variable`. Default is to report mean (SD)
#'     to one decimal place.
#' @param value_discrete (optional) An expression describing how to format the
#'     outcome values for discrete variables. Can refer to n/proportion as
#'     `.n`, `.proportion`, and the name of the variable being formatted as
#'     `.variable`. Default is to report n (percentage) to zero decimal places.
#' @param total (optional) Logical value; should a 'total' (full sample) column
#'     be included? Default is `TRUE` if `by` is specified, `FALSE` otherwise.
#'     Can also be a character string giving the column name to use for the
#'     total column (default is 'Entire sample').
#'
#' @export
create_descriptive_table <- function(
  df, continuous = NULL, discrete = NULL, multiresponse = NULL,
  output = c(names(continuous), names(discrete), names(multiresponse)), by = NULL,
  value_continuous = mean_sd(.mean, .sd, accuracy = 0.1),
  value_discrete = n_percent(.n, .proportion), total = !is.null(by)
) {
  if (!is.data.frame(df)) stop_wrong_type("df", "a data frame")
  if (!is.null(continuous)) {
    if (!rlang::is_bare_character(continuous)) stop_wrong_type("continuous", "a character vector")
    if (has_names(continuous)) {
      nm <- names(continuous)
      names(continuous)[nm == "" | is.na(nm)] <- unname(continuous)[nm == "" | is.na(nm)]
    } else names(continuous) <- continuous
  }
  if (!is.null(discrete)) {
    if (!rlang::is_bare_character(discrete)) stop_wrong_type("discrete", "a character vector")
    if (has_names(discrete)) {
      nm <- names(discrete)
      names(discrete)[nm == "" | is.na(nm)] <- unname(discrete)[nm == "" | is.na(nm)]
    } else names(discrete) <- discrete
  }
  if (!is.null(multiresponse)
      && (!rlang::is_bare_list(multiresponse) || !is_all_character(multiresponse)))
    stop_wrong_type("multiresponse", "a list of character vectors")
  if (!rlang::is_bare_character(output)) stop_wrong_type("output", "a character vector")
  if (!is.null(by) && !rlang::is_string(by)) stop_wrong_type("by", "a string scalar")
  if (!rlang::is_bool(total) && !rlang::is_string(total))
    stop_wrong_type("total", "a logical or string scalar")

  value_continuous <- rlang::enquo(value_continuous)
  value_discrete <- rlang::enquo(value_discrete)

  if (isTRUE(total)) total <- 'Entire sample'
  if (isFALSE(total)) total <- NULL

  variable_names <- c(names(continuous), names(discrete), names(multiresponse))

  continuous_tables <- stats::setNames(lapply(
    seq_along(continuous),
    function(i) continuous_characteristics_table(df, by, continuous[[i]], names(continuous)[[i]],
                                                 value_continuous, total)
  ), names(continuous))
  discrete_tables <- stats::setNames(lapply(
    seq_along(discrete),
    function(i) discrete_characteristics_table(df, by, discrete[[i]], names(discrete)[[i]],
                                               value_discrete, total)
  ), names(discrete))
  multiresponse_tables <- stats::setNames(lapply(
    seq_along(multiresponse),
    function(i)
      multiresponse_characteristics_table(df, by, multiresponse[[i]], names(multiresponse)[[i]],
                                          value_discrete, total)
  ), names(multiresponse))

  header_rows <- lapply(stats::setNames(nm = setdiff(output, variable_names)), make_header_row)

  rlang::exec(rbind,
              !!!(c(continuous_tables, discrete_tables, multiresponse_tables, header_rows)[output]),
              make.row.names = FALSE)
}

continuous_characteristics_table <- function(df, by, variable, name, value, total) {
  by <- if (is.null(by)) "value" else df[[by]]
  values <- lapply(
    split(df, by),
    function(df) rlang::eval_tidy(
      value,
      list(
        .mean = mean(df[[variable]]), .sd = stats::sd(df[[variable]]), .lq = lq(df[[variable]]),
        .median = stats::median(df[[variable]]), .uq = uq(df[[variable]]),
        .max = max(df[[variable]]), .variable = variable
      )
    )
  )
  out <- cbind(data.frame("Patient characteristic" = name, check.names = FALSE),
               as.data.frame(values, optional = TRUE))
  if (!is.null(total)) {
    total_column <- rlang::eval_tidy(
      value,
      list(
        .mean = mean(df[[variable]]), .sd = stats::sd(df[[variable]]), .lq = lq(df[[variable]]),
        .median = stats::median(df[[variable]]), .uq = uq(df[[variable]]),
        .max = max(df[[variable]]), .variable = variable
      )
    )
    out[[total]] <- total_column
  }
  class(out) <- class(df)

  out
}

discrete_characteristics_table <- function(df, by, variable, name, value, total) {
  f <- interaction(df[c(by, variable)])
  grouped <- rlang::exec(rbind,
                         !!!lapply(split(df, f), function(df) {df$n <- nrow(df); df[1, ]}))
  by <- if (is.null(by)) "value" else grouped[[by]]
  values <- lapply(
    split(grouped, by),
    function(df) rlang::eval_tidy(
      value,
      list(.variable = df[[variable]], .n = df$n, .proportion = df$n / sum(df$n))
    )
  )
  out <- cbind(
    data.frame("Patient characteristic" = paste0('&emsp;', levels(as.factor(grouped[[variable]]))),
               check.names = FALSE),
    as.data.frame(values, optional = TRUE)
  )
  if (!is.null(total)) {
    ungrouped <- rlang::exec(
      rbind, !!!lapply(split(df, df[[variable]]), function(df) {df$n <- nrow(df); df[1, ]})
    )
    total_column <- rlang::eval_tidy(
      value,
      list(.variable = ungrouped[[variable]],
           .n = ungrouped$n,
           .proportion = ungrouped$n / sum(ungrouped$n))
    )
    out[[total]] <- total_column
  }
  out <- rbind(make_header_row(name, out), out)
  class(out) <- class(df)

  out
}

multiresponse_characteristics_table <- function(df, by, variables, name, value, total) {
  by <- if (is.null(by)) "value" else df[[by]]

  out <- lapply(
    seq_along(variables),
    function(i) {
      values <- lapply(
        split(df, by),
        function(df) rlang::eval_tidy(
          value,
          list(.n = sum(df[[variables[[i]]]]), .proportion = mean(df[[variables[[i]]]]),
               .variable = variables[[i]])
        )
      )
      out <- cbind(
        data.frame("Patient characteristic" = paste0('&emsp;', names(variables)[[i]]),
                   check.names = FALSE),
        as.data.frame(values, optional = TRUE)
      )
      if (!is.null(total)) {
        out[[total]] <- rlang::eval_tidy(
          value,
          list(.n = sum(df[[variables[[i]]]]), .proportion = mean(df[[variables[[i]]]]),
               .variable = variables[[i]])
        )
      }
      out
    }
  )
  out <- rlang::exec(rbind, !!!out)
  out <- rbind(make_header_row(name, out), out)
  class(out) <- class(df)

  out
}

make_header_row <- function(x, df) {
  out <- df[1, ]
  out[] <- NA
  out[1, 1] = x
  out
}
