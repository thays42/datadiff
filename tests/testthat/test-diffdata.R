test_that("diffdata validates inputs correctly", {
  # Valid inputs should work
  df1 <- tibble(a = 1:5, b = letters[1:5])
  df2 <- tibble(a = c(1:4, 6), b = letters[1:5])

  # Test x must be a tibble
  expect_error(diffdata("not_a_dataframe", df2), "x must be a data frame")
  expect_error(diffdata(NULL, df2), "x must be a data frame")
  expect_error(diffdata(list(a = 1:5), df2), "x must be a data frame")

  # Test y must be a tibble
  expect_error(diffdata(df1, "not_a_dataframe"), "y must be a data frame")
  expect_error(diffdata(df1, NULL), "y must be a data frame")

  # Test x must have at least one row
  expect_error(
    diffdata(tibble(), df2),
    "x must have at least one row"
  )

  # Test y must have at least one row
  expect_error(
    diffdata(df1, tibble()),
    "y must have at least one row"
  )

  # Test max_differences must be numeric and length 1
  expect_error(
    diffdata(df1, df2, max_differences = "ten"),
    "max_differences must be numeric"
  )
  expect_error(
    diffdata(df1, df2, max_differences = 1:3),
    "max_differences must be length 1"
  )

  # Test context_rows must be numeric and length 2
  expect_error(
    diffdata(df1, df2, context_rows = "three"),
    "context_rows must be numeric"
  )
  expect_error(
    diffdata(df1, df2, context_rows = 1:3),
    "context_rows must be length 2"
  )
  expect_error(
    diffdata(df1, df2, context_rows = 3),
    "context_rows must be length 2"
  )

  # Test column differences handling
  df3 <- tibble(a = 1:5, c = letters[1:5]) # different column names
  expect_message(
    result <- diffdata(df1, df3),
    "Cannot diff data with column differences."
  )

  expect_true(is.data.frame(result))
  expect_true(".diff" %in% names(result))
})

test_that("diffdata handles edge cases", {
  # Test with minimal tibbles
  df_min1 <- tibble(a = 1)
  df_min2 <- tibble(a = 2)
  expect_no_error(diffdata(df_min1, df_min2))

  # Test with integer context_rows
  df1 <- tibble(a = 1:5, b = letters[1:5])
  df2 <- tibble(a = c(1:4, 6), b = letters[1:5])
  expect_message(
    diffdata(df1, df2, context_rows = c(0L, 0L)),
    "Cannot diff data with column differences."
  )
  expect_message(
    diffdata(df1, df2, context_rows = c(10L, 5L)),
    "Cannot diff data with column differences."
  )
})
