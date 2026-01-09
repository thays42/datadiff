f_green <- formattable::formatter(
  "span",
  style = "color:green; white-space: nowrap; display: block; overflow: clip; max-width: 200px"
)
f_red <- formattable::formatter(
  "span",
  style = "color:red; white-space: nowrap; display: block; overflow: clip; max-width: 200px"
)
f_ctx <- formattable::formatter(
  "span",
  style = "white-space: nowrap; display: block; overflow: clip; max-width: 200px"
)

#' Render HTML diff
#'
#' @param diffs Data frame as returned by compare_diff (internal)
show_diff <- function(diffs) {
  # Identify blocks of rows for formatting the table
  row_groups <- diffs |>
    mutate(
      .rn = row_number(),
      .block = cumsum(replace_na(.data$.row > lag(.data$.row) + 1, FALSE))
    ) |>
    group_by(.data$.block) |>
    summarize(
      start_row = min(.data$.rn),
      end_row = max(.data$.rn)
    ) |>
    ungroup()

  # Identify row types
  ours <- which(diffs$.source == "x")
  theirs <- which(diffs$.source == "y")
  context <- which(diffs$.diff_type == "context")

  # Format cells
  diffs <- diffs |>
    group_by(.data$.row) |>
    mutate(across(!c(.join_type, .source), function(x) {
      case_when(
        .data$.source == "x" & !is_equal(x, lead(x)) | .data$.join_type == "x" ~
          f_red(x),
        .data$.source == "y" & !is_equal(x, lag(x)) | .data$.join_type == "y" ~
          f_green(x),
        TRUE ~ f_ctx(x)
      )
    })) |>
    ungroup() |>
    select(-c(.join_type, .diff_type, .source))

  # Build table
  tbl <- formattable::formattable(diffs) |>
    kableExtra::kbl(escape = FALSE, row.names = FALSE) |>
    kableExtra::kable_paper(
      full_width = FALSE,
      fixed_thead = TRUE,
      html_font = "monospace"
    ) |>
    kableExtra::column_spec(
      seq_along(diffs),
      border_left = "1px solid #eeeeee",
      border_right = "1px solid #eeeeee"
    ) |>
    kableExtra::row_spec(ours, background = "#e6a8a8") |>
    kableExtra::row_spec(theirs, background = "#a7d1a9") |>
    kableExtra::row_spec(context, color = "#959595")

  for (i in seq_len(nrow(row_groups))) {
    tbl <- kableExtra::pack_rows(
      tbl,
      start_row = row_groups$start_row[i],
      end_row = row_groups$end_row[i]
    )
  }

  tbl
}

#' Render a diff in a flexdashboard
#'
#' @param diff Data frame as returned by compare_data.
#' @export
render_diff <- function(diff) {
  if (!requireNamespace("flexdashboard", quietly = TRUE)) {
    stop(
      "flexdashboard is not installed. Please install it with `install.packages('flexdashboard')`."
    )
  }

  tempdir(TRUE)
  fp <- tempfile()

  diff |>
    show_diff() |>
    saveRDS(fp)

  out <- fs::path_package("datadiff", "report.Rmd") |>
    rmarkdown::render(
      params = list(data = fp),
      output_dir = tempdir(),
      quiet = TRUE
    )

  if (!interactive()) {
    return()
  }

  if (
    requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
  ) {
    rstudioapi::viewer(out)
  } else {
    utils::browseURL(out)
  }
}
