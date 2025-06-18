library(tidyverse)
library(formattable)
library(kableExtra)
library(rlang)
library(glue)


is_equal <- function(x, y) {
  if (is.numeric(x)) {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) < 1e-5)
  } else {
    (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
  }
}


compare_join <- function(x, y) {
  full_join(
    x = mutate(x, .ours = row_number()),
    y = mutate(y, .theirs = row_number()),
    by = join_by(.ours == .theirs),
    suffix = c(".ours", ".theirs"),
    keep = TRUE
  ) |>
    mutate(
      .row = coalesce(.ours, .theirs),
      .type = case_when(
        !is.na(.ours) & !is.na(.theirs) ~ "both",
        !is.na(.ours) ~ "ours",
        !is.na(.theirs) ~ "theirs"
      ),
      .before = everything()
    ) |>
    select(-.ours, -.theirs)
}

compare_diff <- function(data, context = c(3L, 3L), max_differences = Inf) {
  compare_columns <- names(data) |>
    stringr::str_subset("^.+\\.(ours|theirs)$") |>
    stringr::str_remove("\\.(ours|theirs)$") |>
    unique()

  diff_mask <- rep(FALSE, nrow(data))
  for (column in compare_columns) {
    diff_mask <- diff_mask |
      !is_equal(
        data[[paste0(column, ".ours")]],
        data[[paste0(column, ".theirs")]]
      )
  }

  n_differences <- sum(diff_mask)
  if (n_differences > max_differences) {
    cli::cli_alert_info(glue(
      "{n_differences} differences detected. Returning the first {max_differences} differences only."
    ))
    last_diff <- max(head(which(diff_mask), max_differences))
    diff_mask[(last_diff + 1):nrow(data)] <- FALSE
  }

  context_mask <- rep(FALSE, nrow(data))
  diff_indices <- which(diff_mask)
  n_diffs <- length(diff_indices)
  ctx_back <- rep(context[1] + 1, times = n_diffs)
  ctx_fwd <- rep(context[2] + 1, times = n_diffs)

  context_mask[pmax(
    sequence(ctx_back, from = diff_indices, by = -1L),
    1L
  )] <- TRUE
  context_mask[pmin(
    sequence(ctx_fwd, from = diff_indices, by = 1L),
    nrow(data)
  )] <- TRUE
  context_mask[which(diff_mask)] <- FALSE

  context <- data[context_mask, ] |>
    mutate(.type = "context") |>
    select(!all_of(str_c(compare_columns, ".theirs"))) |>
    rename_all(\(x) str_remove(x, "\\.ours$"))

  data[diff_mask, ] |>
    pivot_longer(
      ends_with(".ours") | ends_with(".theirs"),
      names_to = c(".value", ".side"),
      names_pattern = "^(.+)\\.(ours|theirs)$"
    ) |>
    filter(.type == "both" | .type == .side) |>
    mutate(.type = "diff") |>
    bind_rows(context) |>
    select(.row, .type, .side, everything()) |>
    arrange(.row)
}

f_green <- formatter(
  "span",
  style = "color:green; white-space: nowrap; display: block; overflow: clip; max-width: 200px"
)
f_red <- formatter(
  "span",
  style = "color:red; white-space: nowrap; display: block; overflow: clip; max-width: 200px"
)
f_ctx <- formatter(
  "span",
  style = "white-space: nowrap; display: block; overflow: clip; max-width: 200px"
)

show_diff <- function(diffs) {
  row_groups <- diffs |>
    mutate(
      .rn = row_number(),
      .block = cumsum(replace_na(.row > lag(.row) + 1, FALSE))
    ) |>
    group_by(.block) |>
    summarize(
      start_row = min(.rn),
      end_row = max(.rn)
    ) |>
    ungroup()

  ours <- which(diffs$.side == "ours")
  theirs <- which(diffs$.side == "theirs")
  context <- which(diffs$.type == "context")

  diffs <- diffs |>
    group_by(.row) |>
    mutate(across(!c(.type, .side), function(x) {
      case_when(
        .side == "ours" & !is_equal(x, lead(x)) | .type == "ours" ~ f_red(x),
        .side == "theirs" & !is_equal(x, lag(x)) | .type == "theirs" ~
          f_green(x),
        TRUE ~ f_ctx(x)
      )
    })) |>
    ungroup() |>
    select(-c(.row, .type, .side))

  tbl <- formattable(diffs) |>
    kbl(escape = FALSE, row.names = FALSE) |>
    kable_paper(full_width = FALSE, fixed_thead = TRUE, html_font = "monospace") |>
    column_spec(
      seq_along(diffs),
      border_left = "1px solid #eeeeee",
      border_right = "1px solid #eeeeee"
    ) |>
    row_spec(ours, background = "#e6a8a8") |>
    row_spec(theirs, background = "#a7d1a9") |>
    row_spec(context, color = "#959595")

  walk2(row_groups$start_row, row_groups$end_row, function(a, b) {
    tbl <<- pack_rows(tbl, start_row = a, end_row = b)
  })

  tbl
}

diffdata <- function(x, y, max_differences = 10, context = c(3L, 3L)) {
  meta_diff <- diffmeta(x, y)
  if (nrow(meta_diff) > 0) {
    cli::cli_alert_danger("Cannot diff data with column differences.")
    return(meta_diff)
  }

  diff <- compare_join(x, y) |>
    compare_diff(context = context)

  print(show_diff(diff))
  invisible(diff)
}

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
  diff_types <- (x_types[names_in_both] != y_types[names_in_both])
  rc <- bind_rows(
    rc,
    tibble(
      .diff = "type conflict",
      column = names_in_both[diff_types],
      x_type = x_types[diff_types],
      y_type = y_types[diff_types]
    )
  )

  rc
}

col_class <- function(x) {
  x |>
    class() |>
    str_c(collapse = "/")
}

x <- tibble(
  lgl = c(TRUE, TRUE, FALSE, FALSE),
  int = c(1L, 2L, 3L, 4L),
  dbl = c(5.2, 1.2, 3.4, 5.9),
  chr = c("T", "H", "menaadamsmenaadamsmenaadamsmenaadamsmenaadams", "a"),
  dates = lubridate::as_date("2024-01-01") + 0:3,
  datetimes = lubridate::as_datetime("2024-01-01 12:00:00") +
    c(0, 1200, 2400, 3600),
  l2gl = c(TRUE, TRUE, FALSE, FALSE),
  i2nt = c(1L, 2L, 3L, 4L),
  d2bl = c(5.2, 1.2, 3.4, 5.9),
  c2hr = c("T", "H", "m", "a"),
  d2ates = lubridate::as_date("2024-01-01") + 0:3,
  d2atetimes = lubridate::as_datetime("2024-01-01 12:00:00") +
    c(0, 1200, 2400, 3600),
  l3gl = c(TRUE, TRUE, FALSE, FALSE),
  i3nt = c(1L, 2L, 3L, 4L),
  d3bl = c(5.2, 1.2, 3.4, 5.9),
  c3hr = c("T", "H", "m", "a"),
  d3ates = lubridate::as_date("2024-01-01") + 0:3,
  d3atetimes = lubridate::as_datetime("2024-01-01 12:00:00") +
    c(0, 1200, 2400, 3600)
)

y <- x[c(1, 3, 4, 2), ]

ts_x <- tibble(
  date = as.Date("2000-01-01") + 1:365,
  value1 = as.numeric(1:365),
  value2 = rep(1:5, times = 365 / 5),
  value3 = letters[rep(1:5, times = 365 / 5)]
)

ts_y <- ts_x |>
  mutate(
    value1 = if_else(date %in% c("2000-06-05", "2000-10-06"), 50, value1)
  )

diffdata(ts_x, head(ts_y, 360))
