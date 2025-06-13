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

test_that("compare_join works with equal dataframes", {
  df1 <- tibble(a = 1:3, b = letters[1:3])
  df2 <- tibble(a = 1:3, b = letters[1:3])

  result <- compare_join(df1, df2)

  expect_named(result, c(".row", ".join_type", "a.x", "b.x", "a.y", "b.y"))
  expect_equal(result$.join_type, rep("both", 3))
  expect_equal(result$.row, 1:3)
})

test_that("compare_join handles x longer than y", {
  df1 <- tibble(a = 1:5, b = letters[1:5])
  df2 <- tibble(a = 4:6, b = letters[4:6])

  result <- compare_join(df1, df2)

  expect_equal(nrow(result), 5)
  expect_equal(result$.join_type[1:3], rep("both", 3))
  expect_equal(result$.join_type[4:5], rep("x", 2))
})

test_that("compare_join handles y longer than x", {
  df1 <- tibble(a = 4:6, b = letters[4:6])
  df2 <- tibble(a = 1:5, b = letters[1:5])

  result <- compare_join(df1, df2)

  expect_equal(nrow(result), 5)
  expect_equal(result$.join_type[1:3], rep("both", 3))
  expect_equal(result$.join_type[4:5], rep("y", 2))
})

test_that("compare_join handles empty dataframes", {
  df1 <- tibble(a = numeric(0), b = character(0))
  df2 <- tibble(c = numeric(0), d = character(0))

  result <- compare_join(df1, df2)

  expect_equal(nrow(result), 0)
  expect_true(all(c(".row", ".join_type") %in% names(result)))
})
