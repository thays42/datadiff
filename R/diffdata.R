#' Diff Data Frames
#'
#' @param x,y Data frames to diff.
#' @param max_differences Maximum differences to detect.
#' @param context Number of observations before and after difference rows to
#'   contextualize differences.
#' @export
diffdata <- function(x, y, max_differences=10, context=c(3L, 3L)) {
  meta_diff <- diffmeta(x, y)
  if (nrow(meta_diff) > 0) {
    cli::cli_alert_danger("Cannot diff data with column differences.")
    return(meta_diff)
  }

  diff <- compare(x, y, context=context, max_differences=max_differences) |>
    render_diff()

  invisible(diff)
}

#' Diff Data Frames Metadata
#'
#' @param x,y Data frames to diff.
#' @return A data frame of column metadata differences between
#'   `x` and `y`.
#' @export
diffmeta <- function(x, y) {
  rc <- tibble()

  # column names
  x_names <- names(x)
  x_types <- map_chr(x, col_class) |>
    set_names(x_names)
  y_names <- names(y)
  y_types <- map_chr(y, col_class) |>
    set_names(y_names)

  if (!setequal(x_names, y_names)) {
    x_only_names <- setdiff(x_names, y_names)
    if (length(x_only_names) > 0) {
      rc <- bind_rows(rc, tibble(
        .diff = "in x only",
        column = x_only_names,
        x_type = x_types[x_only_names]
      ))
    }
    y_only_names <- setdiff(y_names, x_names)
    if (length(y_only_names) > 0) {
      rc <- bind_rows(rc, tibble(
        .diff = "in y only",
        column = y_only_names,
        y_type = y_types[y_only_names]
      ))
    }
  }

  names_in_both <- intersect(x_names, y_names)

  # column types
  diff_types <- names(which(x_types[names_in_both] != y_types[names_in_both]))
  rc <- bind_rows(rc,tibble(
    .diff = "type conflict",
    column = diff_types,
    x_type = x_types[diff_types],
    y_type = y_types[diff_types]
  ))

  rc |>
    mutate(across(everything(), unname))
}
