test_that("Metadata diff works on equal data frames", {
  expect_equal(nrow(diffmeta(example_types, example_types)), 0L)
})

test_that("Metadata diff works on different data frames", {
  a <- example_types |>
    select(-datetimes)
  b <- example_types |>
    mutate(lgl = as.character(lgl)) |>
    select(-dates)

  actual <- diffmeta(a, b)
  expected <- tribble(
    ~.diff, ~column, ~x_type, ~y_type,
    "in x only", "dates", "Date", NA_character_,
    "in y only", "datetimes", NA_character_, "POSIXct/POSIXt",
    "type conflict", "lgl", "logical" , "character"
  )
  expect_equal(actual, expected)
})
