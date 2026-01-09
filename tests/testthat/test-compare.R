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

  expect_named(
    result,
    c(
      ".row",
      ".join_type",
      "a.__datadiff_x__",
      "b.__datadiff_x__",
      "a.__datadiff_y__",
      "b.__datadiff_y__"
    )
  )
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

test_that("compare_groups works with equal dataframes", {
  df1 <- tibble(group = c("A", "B", "C"), value = 1:3)
  df2 <- tibble(group = c("A", "B", "C"), value = 4:6)

  result <- compare_groups(df1, df2, group)

  # Should return empty since all groups are in both dataframes
  expect_equal(nrow(result), 0)
  expect_named(result, c("group", "in_x", "in_y"))
})

test_that("compare_groups identifies groups only in x or y", {
  df1 <- tibble(group = c("A", "B", "C"), value = 1:3)
  df2 <- tibble(group = c("A", "D"), value = 4:5)

  result <- compare_groups(df1, df2, group)

  expected <- tibble(
    group = c("B", "C", "D"),
    in_x = c(TRUE, TRUE, FALSE),
    in_y = c(FALSE, FALSE, TRUE)
  )
  expect_equal(result, expected)
})

test_that("compare_groups identifies groups only in y", {
  df1 <- tibble(group = c("A", "B"), value = 1:2)
  df2 <- tibble(group = c("A", "C", "D"), value = 3:5)

  result <- compare_groups(df1, df2, group)

  expected <- tibble(
    group = c("B", "C", "D"),
    in_x = c(TRUE, FALSE, FALSE),
    in_y = c(FALSE, TRUE, TRUE)
  )
  expect_equal(result, expected)
})

test_that("compare_groups works with multiple grouping columns", {
  df1 <- tibble(
    group1 = c("A", "A", "B"),
    group2 = c("X", "Y", "X"),
    value = 1:3
  )
  df2 <- tibble(
    group1 = c("A", "B", "C"),
    group2 = c("X", "Y", "X"),
    value = 4:6
  )

  result <- compare_groups(df1, df2, c(group1, group2))

  expect_equal(nrow(result), 4)
  expect_equal(result$group1, c("A", "B", "B", "C"))
  expect_equal(result$group2, c("Y", "X", "Y", "X"))
  expect_equal(result$in_x, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(result$in_y, c(FALSE, FALSE, TRUE, TRUE))
})

test_that("compare_groups works with tidy-select syntax", {
  df1 <- tibble(
    group = c("A", "B", "C"),
    other = c("X", "Y", "Z"),
    value = 1:3
  )
  df2 <- tibble(
    group = c("A", "D"),
    other = c("X", "W"),
    value = 4:5
  )

  result <- compare_groups(df1, df2, starts_with("group"))

  expect_equal(nrow(result), 3)
  expect_equal(result$group, c("B", "C", "D"))
  expect_equal(result$in_x, c(TRUE, TRUE, FALSE))
  expect_equal(result$in_y, c(FALSE, FALSE, TRUE))
})

test_that("compare_groups handles empty dataframes", {
  df1 <- tibble(group = character(0), value = numeric(0))
  df2 <- tibble(group = c("A", "B"), value = 1:2)

  result <- compare_groups(df1, df2, group)

  expect_equal(nrow(result), 2)
  expect_equal(result$group, c("A", "B"))
  expect_equal(result$in_x, c(FALSE, FALSE))
  expect_equal(result$in_y, c(TRUE, TRUE))
})

test_that("compare_groups handles both empty dataframes", {
  df1 <- tibble(group = character(0), value = numeric(0))
  df2 <- tibble(group = character(0), value = numeric(0))

  result <- compare_groups(df1, df2, group)

  expect_equal(nrow(result), 0)
  expect_named(result, c("group", "in_x", "in_y"))
})

test_that("compare_groups works with multiple grouping columns (all unique)", {
  df1 <- tibble(
    group1 = c("A", "A", "B"),
    group2 = c("X", "Y", "X"),
    value = 1:3
  )
  df2 <- tibble(
    group1 = c("A", "B", "C"),
    group2 = c("X", "Y", "X"),
    value = 4:6
  )

  result <- compare_groups(df1, df2, c(group1, group2))

  expected <- tibble(
    group1 = c("A", "B", "B", "C"),
    group2 = c("Y", "X", "Y", "X"),
    in_x = c(TRUE, TRUE, FALSE, FALSE),
    in_y = c(FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(result, expected)
})

test_that("compare_groups works with tidy-select syntax (all unique)", {
  df1 <- tibble(
    group = c("A", "B", "C"),
    other = c("X", "Y", "Z"),
    value = 1:3
  )
  df2 <- tibble(
    group = c("A", "D"),
    other = c("X", "W"),
    value = 4:5
  )

  result <- compare_groups(df1, df2, starts_with("group"))

  expected <- tibble(
    group = c("B", "C", "D"),
    in_x = c(TRUE, TRUE, FALSE),
    in_y = c(FALSE, FALSE, TRUE)
  )
  expect_equal(result, expected)
})

test_that("compare_data respects tolerance parameter", {
  df1 <- tibble(a = c(1.0, 2.0, 3.0), b = c("x", "y", "z"))
  df2 <- tibble(a = c(1.001, 2.0, 3.0), b = c("x", "y", "z"))

  # With default tolerance, small difference should be detected
  result_default <- compare_data(df1, df2, context_rows = c(0L, 0L))
  expect_true(nrow(result_default) > 0)

  # With larger tolerance, difference should be ignored
  result_tolerant <- compare_data(
    df1,
    df2,
    context_rows = c(0L, 0L),
    tolerance = 0.01
  )
  expect_equal(nrow(result_tolerant), 0)
})

test_that("compare_data with tolerance handles multiple numeric columns", {
  df1 <- tibble(a = c(1.0, 2.0), b = c(10.0, 20.0))
  df2 <- tibble(a = c(1.0005, 2.0), b = c(10.0, 20.0005))

  # With tight tolerance, both columns should show differences
  result_tight <- compare_data(
    df1,
    df2,
    context_rows = c(0L, 0L),
    tolerance = 0.0001
  )
  expect_true(nrow(result_tight) > 0)

  # With loose tolerance, no differences
  result_loose <- compare_data(
    df1,
    df2,
    context_rows = c(0L, 0L),
    tolerance = 0.001
  )
  expect_equal(nrow(result_loose), 0)
})

test_that("compare_data handles columns ending in .x or .y", {
  # Columns named value.x and value.y should work correctly
  df1 <- tibble(value.x = 1:3, col = letters[1:3])
  df2 <- tibble(value.x = c(1L, 99L, 3L), col = letters[1:3])

  result <- compare_data(df1, df2, context_rows = c(0L, 0L))

  # Should detect the difference in value.x at row 2
  expect_true(nrow(result) > 0)
  expect_true("value.x" %in% names(result))

  # The difference should be at row 2
  diff_rows <- result[result$.diff_type == "diff", ]
  expect_true(all(diff_rows$.row == 2))
})

test_that("compare_join handles columns ending in .x or .y", {
  df1 <- tibble(value.x = 1:3, value.y = 4:6)
  df2 <- tibble(value.x = 1:3, value.y = 4:6)

  result <- compare_join(df1, df2)

  # Should have internal suffixes, not collision with user column names
  expect_true("value.x.__datadiff_x__" %in% names(result))
  expect_true("value.y.__datadiff_x__" %in% names(result))
  expect_true("value.x.__datadiff_y__" %in% names(result))
  expect_true("value.y.__datadiff_y__" %in% names(result))
})
