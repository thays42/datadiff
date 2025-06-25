#' Compare two data frames
#'
#' @param x,y Data frames to compare
#' @return Data frame of `x`, `y` full joined by row number. Shared variables
#'   have suffixes as specified by `suffix`. A `.row` helper variable indicates
#'   the row number. A `.type` helper variable indicates whether the row is in
#'   `x only`, `y only` or `both` data frames.
#' @param context_rows Integer vector of length two indicating the number of context
#'   row to include before and after a difference row.
#' @param context_cols <[`tidy-select`][dplyr_tidy_select]> Columns to include as context.
#' @param max_differences Maximum number of differences to return.
#' @return Data frame of observations that are different in `x` and `y`, or
#'   observations that are in only `x` or `y`, along with context rows.
#' @export
compare_data <- function(
  x,
  y,
  context_rows = c(3L, 3L),
  context_cols = everything(),
  max_differences = Inf
) {
  compare_join(x, y) |>
    compare_diff(
      context_rows = context_rows,
      context_cols = context_cols,
      max_differences = max_differences
    )
}

compare_join <- function(x, y) {
  full_join(
    x = mutate(x, .rn = row_number()),
    y = mutate(y, .rn = row_number()),
    by = join_by(.rn),
    keep = TRUE
  ) |>
    mutate(
      .row = coalesce(.data$.rn.x, .data$.rn.y),
      .join_type = case_when(
        !is.na(.data$.rn.x) & !is.na(.data$.rn.y) ~ "both",
        !is.na(.data$.rn.x) ~ "x",
        !is.na(.data$.rn.y) ~ "y"
      ),
      .before = everything()
    ) |>
    select(-.rn.x, -.rn.y)
}

compare_diff <- function(
  data,
  context_rows = c(3L, 3L),
  context_cols = everything(),
  max_differences = Inf
) {
  # identify columns to compare
  compare_cols <- names(data) |>
    str_subset(str_c("^.+(\\.x|\\.y)$")) |>
    str_remove(str_c("(\\.x|\\.y)$")) |>
    unique()

  # identify rows with differences
  mask <- matrix(FALSE, nrow = nrow(data), ncol = length(compare_cols))
  colnames(mask) <- compare_cols
  for (column in compare_cols) {
    mask[, column] <- !is_equal(
      data[[paste0(column, ".x")]],
      data[[paste0(column, ".y")]]
    )
  }

  # limit to max differences
  row_mask <- apply(mask, 1, any)
  n_differences <- sum(row_mask)
  if (n_differences > max_differences) {
    cli::cli_alert_info(glue(
      "{n_differences} differences detected. Reporting the first {max_differences} differences only."
    ))
    last_diff <- max(head(which(row_mask), max_differences))
    row_mask[(last_diff + 1):nrow(data)] <- FALSE
    col_mask <- apply(mask, 2, function(x) {
      any(head(x, last_diff))
    })
  } else {
    col_mask <- apply(mask, 2, any)
  }
  diff_columns <- compare_cols[col_mask]

  # identify context rows
  context_mask <- rep(FALSE, nrow(data))
  diff_indices <- which(row_mask)
  n_diffs <- length(diff_indices)
  ctx_back <- rep(context_rows[1] + 1, times = n_diffs)
  ctx_fwd <- rep(context_rows[2] + 1, times = n_diffs)
  context_mask[pmax(
    sequence(ctx_back, from = diff_indices, by = -1L),
    1L
  )] <- TRUE
  context_mask[pmin(
    sequence(ctx_fwd, from = diff_indices, by = 1L),
    nrow(data)
  )] <- TRUE
  context_mask[which(row_mask)] <- FALSE

  # pull context rows
  # context rows are pulled from the `x` data frame
  # drop `y` data frame columns and de-suffix `x` data frame columns
  context_data <- data[context_mask, ] |>
    mutate(.diff_type = "context") |>
    select(!all_of(str_c(compare_cols, ".y"))) |>
    rename_all(\(x) str_remove(x, "\\.x$"))

  # pull data rows
  data[row_mask, ] |>
    # pivot so that `x` rows stacked on `y` rows.
    pivot_longer(
      ends_with(".x") | ends_with(".y"),
      names_to = c(".value", ".source"),
      names_pattern = str_c("^(.+)\\.(x|y)$")
    ) |>

    # remove empty rows representing rows in x not in y or vice versa
    filter(.data$.join_type == "both" | .data$.join_type == .data$.source) |>
    mutate(.diff_type = "diff") |>

    # add context rows, arrange columns and rows for output
    bind_rows(context_data) |>
    select(
      .row,
      .join_type,
      .diff_type,
      .source,
      all_of(context_cols),
      all_of(diff_columns)
    ) |>
    arrange(.data$.row)
}

#' Compare groups between two data frames
#'
#' @param x,y Data frames to compare
#' @param group_cols <[`tidy-select`][dplyr_tidy_select]> Columns to use for grouping
#' @return A data frame containing the grouping columns and two additional columns,
#'   `in_x` and `in_y`, which are TRUE if the group values are in the corresponding
#'   data frame and FALSE otherwise. Records where both `in_x` and `in_y` are TRUE
#'   are excluded from the output.
#' @export
compare_groups <- function(x, y, group_cols) {
  x_groups <- x |> select({{ group_cols }}) |> distinct() |> mutate(in_x = TRUE)
  y_groups <- y |> select({{ group_cols }}) |> distinct() |> mutate(in_y = TRUE)
  join_cols <- setdiff(names(x_groups), "in_x")

  full_join(x_groups, y_groups, by = join_cols) |>
    filter(is.na(.data$in_x) | is.na(.data$in_y)) |>
    mutate(
      in_x = replace_na(.data$in_x, FALSE),
      in_y = replace_na(.data$in_y, FALSE)
    ) |>
    arrange(pick(all_of(join_cols)))
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
      rc <- bind_rows(
        rc,
        tibble(
          .diff = "in x only",
          column = x_only_names,
          x_type = x_types[x_only_names]
        )
      )
    }
    y_only_names <- setdiff(y_names, x_names)
    if (length(y_only_names) > 0) {
      rc <- bind_rows(
        rc,
        tibble(
          .diff = "in y only",
          column = y_only_names,
          y_type = y_types[y_only_names]
        )
      )
    }
  }

  names_in_both <- intersect(x_names, y_names)

  # column types
  diff_types <- names(which(x_types[names_in_both] != y_types[names_in_both]))
  rc <- bind_rows(
    rc,
    tibble(
      .diff = "type conflict",
      column = diff_types,
      x_type = x_types[diff_types],
      y_type = y_types[diff_types]
    )
  )

  rc |>
    mutate(across(everything(), unname))
}
