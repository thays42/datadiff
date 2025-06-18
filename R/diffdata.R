#' Diff Data Frames
#'
#' @param x,y Data frames to diff.
#' @param max_differences Maximum differences to detect.
#' @param context_rows Integer vector of length two indicating the number of context
#'   row to include before and after a difference row.
#' @param context_cols <[`tidy-select`][dplyr_tidy_select]> Columns to include as context.
#' @param max_differences Maximum number of differences to return.
#' @return Data frame of observations that are different in `x` and `y`, or
#'   observations that are in only `x` or `y`, along with context rows.
#' @export
diffdata <- function(
  x,
  y,
  max_differences = 10,
  context_rows = c(3L, 3L),
  context_cols = everything()
) {

  stopifnot(
    "x must be a data frame" = is.data.frame(x),
    "x must have at least one row" = nrow(x) > 0,
    "y must be a data frame" = is.data.frame(y),
    "y must have at least one row" = nrow(y) > 0,
    "max_differences must be numeric" = is.numeric(max_differences),
    "max_differences must be length 1" = length(max_differences) == 1,
    "context_rows must be numeric" = is.numeric(context_rows),
    "context_rows must be length 2" = length(context_rows) == 2
  )

  max_differences <- as.integer(max_differences)
  context_rows <- as.integer(context_rows)

  col_diff <- compare_columns(x, y)
  if (nrow(col_diff) > 0) {
    cli::cli_alert_danger("Cannot diff data with column differences.")
    return(col_diff)
  }

  data_diff <- compare_data(
    x,
    y,
    context_rows = context_rows,
    context_cols = context_cols,
    max_differences = max_differences
  ) |>
    render_diff()

  invisible(data_diff)
}
