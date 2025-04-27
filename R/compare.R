#' Join two data frames for comparison
#'
#' @param x,y Data frames to compare
#' @param suffix Two element character vector of suffixes to disambiguate columns
#'   from `x`, `y`.
#' @return Data frame of `x`, `y` full joined by row number. Shared variables
#'   have suffixes as specified by `suffix`. A `.row` helper variable indicates
#'   the row number. A `.type` helper variable indicates whether the row is in
#'   `x only`, `y only` or `both` data frames.
#' @export
compare_join <- function(x, y, suffix = c(".x", ".y")) {
  suffix_x <- suffix[1]
  suffix_y <- suffix[2]
  id_x <- str_c(".id", suffix_x)
  id_y <- str_c(".id", suffiy_y)
  full_join(
    x = mutate(x, "{{id_x}}" := row_number()),
    y = mutate(y, "{{id_y}}" := row_number()),
    by = c(id_x = id_y),
    suffix = suffix,
    keep = TRUE
  ) |>
    mutate(
      .row = coalesce(.data[[id_x]], .data[[id_y]]),
      .type = case_when(
        !is.na(.data[[id_x]]) & !is.na(.data[[id_y]]) ~ "both",
        !is.na(.data[[id_x]]) ~ "x only",
        !is.na(.data[[id_y]]) ~ "y only"
      ),
      .before = everything()
    ) |>
    select(!all_of(c(id_x, id_y)))
}


#' Diff data frames that have been compare joined
#'
#' @param data Data frame as returned by [compare_join].
#' @param suffix Two element character vector of suffixes to disambiguate columns
#'   from data frames joined in [compare_join].
#' @param context Integer vector of length two indicating the number of context
#'   row to include before and after a difference row.
#' @param max_differences Maximum number of differences to return.
compare_diff <- function(data, suffix = c(".x", ".y"), context = c(3L,3L), max_differences = Inf) {
  # identify columns to compare
  suffix_pattern <- str_c(suffix, collapse = "|")
  compare_columns <- names(data) |>
    str_subset(str_c("^.+\\.(", suffix_pattern, ")$")) |>
    str_remove(str_c("\\.(, ", suffix_pattern, ")$")) |>
    unique()

  # identify rows with differences
  diff_mask <- rep(FALSE, nrow(data))
  for (column in compare_columns) {
    diff_mask <- diff_mask | !is_equal(data[[paste0(column, suffix[1])]], data[[paste0(column, suffix[2])]])
  }

  # limit to max differences
  n_differences <- sum(diff_mask)
  if (n_differences > max_differences) {
    cli::cli_alert_info(glue("{n_differences} differences detected. Returning the first {max_differences} differences only."))
    last_diff <- max(head(which(diff_mask), max_differences))
    diff_mask[(last_diff+1):nrow(data)] <- FALSE
  }

  # identify context rows
  context_mask <- rep(FALSE, nrow(data))
  diff_indices <- which(diff_mask)
  n_diffs <- length(diff_indices)
  ctx_back <- rep(context[1]+1, times = n_diffs)
  ctx_fwd <- rep(context[2]+1, times = n_diffs)
  context_mask[pmax(sequence(ctx_back, from=diff_indices, by=-1L), 1L)] <- TRUE
  context_mask[pmin(sequence(ctx_fwd, from=diff_indices, by=1L), nrow(data))] <- TRUE
  context_mask[which(diff_mask)] <- FALSE

  # pull context rows
  # context rows are pulled from the `x` data frame
  # drop `y` data frame columns and de-suffix `x` data frame columns
  context <- data[context_mask,] |>
    mutate(.type = "context") |>
    select(!all_of(str_c(compare_columns, suffix[2]))) |>
    rename_all(\(x) str_remove(x, str_c("\\", suffix[1], "$")))

  # pull data rows
  data[diff_mask,] |>
    # pivot so that `x` rows stacked on `y` rows.
    pivot_longer(
      ends_with(suffix[1]) | ends_with(suffix[2]),
      names_to = c(".value", ".side"),
      names_pattern = str_c("^(.+)\\.(, ", suffix_pattern, ")$")
    ) |>

    # remove empty rows representing rows in x not in y or vice versa
    filter(.type == "both" | .type == .side) |>
    mutate(.type = "diff") |>

    # add context rows, arrange columns and rows for output
    bind_rows(context) |>
    select(.row, .type, .side, everything()) |>
    arrange(.row)
}
