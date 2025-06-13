test_that("diffdata validates inputs correctly", {
  # Valid inputs should work
  df1 <- tibble(a = 1:5, b = letters[1:5])
  df2 <- tibble(a = c(1:4, 6), b = letters[1:5])

  # Test x must be a tibble
  expect_error(diffdata("not_a_dataframe", df2), "must be class")
  expect_error(diffdata(NULL, df2), "must be class")
  expect_error(diffdata(list(a = 1:5), df2), "must be class")

  # Test y must be a tibble
  expect_error(diffdata(df1, "not_a_dataframe"), "must be class")
  expect_error(diffdata(df1, NULL), "must be class")

  # Test x must have at least one row
  expect_error(
    diffdata(tibble(), df2),
    "nrow.*must be at least"
  )

  # Test y must have at least one row
  expect_error(
    diffdata(df1, tibble()),
    "nrow.*must be at least"
  )

  # Test max_differences must be numeric and length 1
  expect_error(diffdata(df1, df2, max_differences = "ten"), "must be of class")
  expect_error(diffdata(df1, df2, max_differences = 1:3), "must have length")

  # Test context_rows must be numeric and length 2
  expect_error(diffdata(df1, df2, context_rows = "three"), "must be of class")
  expect_error(diffdata(df1, df2, context_rows = 1:3), "must have length")
  expect_error(diffdata(df1, df2, context_rows = 3), "must have length")

  # Test column differences handling
  df3 <- tibble(a = 1:5, c = letters[1:5]) # different column names
  result <- diffdata(df1, df3)
  expect_true(inherits(result, "tibble"))
  expect_true("column_diff" %in% names(result))
})

test_that("diffdata handles edge cases", {
  # Test with minimal tibbles
  df_min1 <- tibble(a = 1)
  df_min2 <- tibble(a = 2)
  expect_no_error(diffdata(df_min1, df_min2))

  # Test with identical tibbles
  df_same <- tibble(a = 1:5, b = letters[1:5])
  expect_no_error(diffdata(df_same, df_same))

  # Test with integer context_rows
  df1 <- tibble(a = 1:5, b = letters[1:5])
  df2 <- tibble(a = c(1:4, 6), b = letters[1:5])
  expect_no_error(diffdata(df1, df2, context_rows = c(0L, 0L)))
  expect_no_error(diffdata(df1, df2, context_rows = c(10L, 5L)))
})
