#' Diff Data Frames
#'
#' @param x,y Data frames to diff.
#' @param max_differences Maximum differences to detect.
#' @param context Number of observations before and after difference rows to
#'   contextualize differences.
#' @export
diffdata <- function(x, y, max_differences=10, context=c(3L, 3L)) {
  col_diff <- compare_columns(x, y)
  if (nrow(col_diff) > 0) {
    cli::cli_alert_danger("Cannot diff data with column differences.")
    return(col_diff)
  }

  data_diff <- compare_data(x, y, context=context, max_differences=max_differences) |>
    render_diff()

  invisible(data_diff)
}
