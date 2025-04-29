#' Compare two data frames
#'
#' @param x,y Data frames to compare
#' @return Data frame of `x`, `y` full joined by row number. Shared variables
#'   have suffixes as specified by `suffix`. A `.row` helper variable indicates
#'   the row number. A `.type` helper variable indicates whether the row is in
#'   `x only`, `y only` or `both` data frames.
#' @param context_rows Integer vector of length two indicating the number of context
#'   row to include before and after a difference row.
#' @param max_differences Maximum number of differences to return.
#' @return Data frame of observations that are different in `x` and `y`, or
#'   observations that are in only `x` or `y`, along with context rows.
#' @export
compare_data <- function(x, y, context_rows = c(3L, 3L), max_differences = Inf) {
  compare_join(x, y) |>
    compare_diff(context_rows = context_rows, max_differences = max_differences)
}

compare_join <- function(x, y) {
  full_join(
    x = mutate(x, .rn = row_number()),
    y = mutate(y, .rn = row_number()),
    by = join_by(.rn),
    keep = TRUE
  ) |>
    mutate(
      .row = coalesce(.rn.x, .rn.y),
      .join_type = case_when(
        !is.na(.rn.x) & !is.na(.rn.y) ~ "both",
        !is.na(.rn.x) ~ "x",
        !is.na(.rn.y) ~ "y"
      ),
      .before = everything()
    ) |>
    select(-.rn.x, -.rn.y)
}

#' Diff data frames that have been compare joined
#'
#' @param data Data frame as returned by [compare_join].
#' @param context_rows Integer vector of length two indicating the number of context
#'   row to include before and after a difference row.
#' @param max_differences Maximum number of differences to return.
compare_diff <- function(data, context_rows = c(3L,3L), max_differences = Inf) {
  # identify columns to compare
  compare_columns <- names(data) |>
    str_subset(str_c("^.+(\\.x|\\.y)$")) |>
    str_remove(str_c("(\\.x|\\.y)$")) |>
    unique()

  # identify rows with differences
  diff_mask <- rep(FALSE, nrow(data))
  for (column in compare_columns) {
    diff_mask <- diff_mask | !is_equal(data[[paste0(column, ".x")]], data[[paste0(column, ".y")]])
  }

  # limit to max differences
  n_differences <- sum(diff_mask)
  if (n_differences > max_differences) {
    cli::cli_alert_info(glue("{n_differences} differences detected. Reporting the first {max_differences} differences only."))
    last_diff <- max(head(which(diff_mask), max_differences))
    diff_mask[(last_diff+1):nrow(data)] <- FALSE
  }

  # identify context rows
  context_mask <- rep(FALSE, nrow(data))
  diff_indices <- which(diff_mask)
  n_diffs <- length(diff_indices)
  ctx_back <- rep(context_rows[1]+1, times = n_diffs)
  ctx_fwd <- rep(context_rows[2]+1, times = n_diffs)
  context_mask[pmax(sequence(ctx_back, from=diff_indices, by=-1L), 1L)] <- TRUE
  context_mask[pmin(sequence(ctx_fwd, from=diff_indices, by=1L), nrow(data))] <- TRUE
  context_mask[which(diff_mask)] <- FALSE

  # pull context rows
  # context rows are pulled from the `x` data frame
  # drop `y` data frame columns and de-suffix `x` data frame columns
  context <- data[context_mask,] |>
    mutate(.diff_type = "context") |>
    select(!all_of(str_c(compare_columns, ".y"))) |>
    rename_all(\(x) str_remove(x, str_c("\\.x$")))

  # pull data rows
  data[diff_mask,] |>
    # pivot so that `x` rows stacked on `y` rows.
    pivot_longer(
      ends_with(".x") | ends_with(".y"),
      names_to = c(".value", ".source"),
      names_pattern = str_c("^(.+)\\.(x|y)$")
    ) |>

    # remove empty rows representing rows in x not in y or vice versa
    filter(.join_type == "both" | .join_type == .source) |>
    mutate(.diff_type = "diff") |>

    # add context rows, arrange columns and rows for output
    bind_rows(context) |>
    select(.row, .join_type, .diff_type, .source, everything()) |>
    arrange(.row)
}

#' Compare column metadata between two data frames
#'
#' @param x,y Data frames to compare.
#' @return A data frame of column metadata differences between
#'   `x` and `y`.
#' @export
compare_columns <- function(x, y) {
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
