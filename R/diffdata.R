#' Diff Data Frames
#'
#' @param x,y Data frames to diff.
#' @param max_differences Maximum differences to detect.
#' @param context_rows Number of observations before and after difference rows to
#'   contextualize differences.
#' @export
diffdata <- function(
  x,
  y,
  max_differences = 10,
  context_rows = c(3L, 3L),
  context_cols = everything()
) {
  assert_class(x, "data.frame")
  assert_class(y, "data.frame")
  max_differences <- max_differences |>
    assert_class(c("numeric", "integer")) |>
    assert_length(1L) |>
    as.integer()
  context_rows <- context_rows |>
    assert_class(c("numeric", "integer")) |>
    assert_length(2L) |>
    as.integer()

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
