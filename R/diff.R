f_green <- formattable::formatter("span", style = "color:green; white-space: nowrap; display: block; overflow: clip; max-width: 200px")
f_red <- formattable::formatter("span", style = "color:red; white-space: nowrap; display: block; overflow: clip; max-width: 200px")
f_ctx <- formattable::formatter("span", style = "white-space: nowrap; display: block; overflow: clip; max-width: 200px")

#' Render HTML diff
#'
#' @param diffs Data frame as returned by [compare_diff]
show_diff <- function(diffs, suffix = c(".x", ".y")) {
  # Identify blocks of rows for formatting the table
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

  # Identify row types
  ours <- which(diffs$.side == suffix[1])
  theirs <- which(diffs$.side == suffix[2])
  context <- which(diffs$.type == "context")

  # Format cells
  diffs <- diffs |>
    group_by(.row) |>
    mutate(across(!c(.type, .side), function(x) {
      case_when(
        .side == suffix[1] & !is_equal(x, lead(x)) | .type == suffix[1] ~ f_red(x),
        .side == suffix[2] & !is_equal(x, lag(x)) | .type == suffix[2] ~ f_green(x),
        TRUE ~ f_ctx(x)
      )
    })) |>
    ungroup() |>
    select(-c(.type, .side))

  # Build table
  tbl <- formattable::formattable(diffs) |>
    kableExtra::kbl(escape = FALSE, row.names = FALSE) |>
    kableExtra::kable_paper(full_width = FALSE, fixed_thead = T, html_font = "monospace") |>
    kableExtra::column_spec(1:ncol(diffs), border_left = "1px solid #eeeeee", border_right = "1px solid #eeeeee") |>
    kableExtra::row_spec(ours, background = "#e6a8a8") |>
    kableExtra::row_spec(theirs, background = "#a7d1a9") |>
    kableExtra::row_spec(context, color = "#959595")

  walk2(row_groups$start_row, row_groups$end_row, function(a, b) {
    tbl <<- kableExtra::pack_rows(tbl, start_row=a, end_row=b)
  })

  tbl
}

render_diff <- function(diff) {
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
  rstudioapi::viewer(out)
}
