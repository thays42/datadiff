test_that("Compare columns returns empty on equal data frames", {
  expect_equal(nrow(compare_columns(example_types, example_types)), 0L)
})

test_that("Compare columns works on different data frames", {
  a <- example_types |>
    select(-datetimes)
  b <- example_types |>
    mutate(lgl = as.character(lgl)) |>
    select(-dates)

  actual <- compare_columns(a, b)
  # fmt: skip
  expected <- tribble(
    ~.diff, ~column, ~x_type, ~y_type,
    "in x only", "dates", "Date", NA_character_,
    "in y only", "datetimes", NA_character_, "POSIXct/POSIXt",
    "type conflict", "lgl", "logical", "character"
  )
  expect_equal(actual, expected)
})
