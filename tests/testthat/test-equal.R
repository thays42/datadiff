test_that("is_equal handles numeric values correctly", {
  # Equal numeric values
  expect_true(is_equal(5, 5))
  expect_true(is_equal(5.0, 5.0))
  expect_true(is_equal(5.0001, 5, tol = 0.001))

  # Different numeric values
  expect_false(is_equal(5, 6))
  expect_false(is_equal(5.0, 5.1))
  expect_false(is_equal(5.0001, 5, tol = 0.00001))

  # Custom tolerance
  expect_true(is_equal(0.999, 1.0, tol = 0.01))
  expect_false(is_equal(0.999, 1.0, tol = 0.0001))
})

test_that("is_equal handles non-numeric values correctly", {
  # Character values
  expect_true(is_equal("a", "a"))
  expect_false(is_equal("a", "b"))

  # Logical values
  expect_true(is_equal(TRUE, TRUE))
  expect_false(is_equal(TRUE, FALSE))

  # Factors
  expect_true(is_equal(factor("a"), factor("a")))
  expect_false(is_equal(factor("a"), factor("b")))
})

test_that("is_equal handles NA values correctly", {
  # NA values
  expect_true(is_equal(NA, NA))
  expect_true(is_equal(NA_real_, NA_real_))
  expect_true(is_equal(NA_character_, NA_character_))

  # NA with non-NA
  expect_false(is_equal(NA, 5))
  expect_false(is_equal(5, NA))
  expect_false(is_equal(NA, "a"))
  expect_false(is_equal("a", NA))
})

test_that("is_equal handles mixed types correctly", {
  # Mixed types should be considered different
  expect_false(is_equal(5, "5"))
  expect_false(is_equal(TRUE, 1))
  expect_false(is_equal(FALSE, 0))
})

test_that("is_equal handles edge cases correctly", {
  # Inf values
  expect_true(is_equal(Inf, Inf))
  expect_true(is_equal(-Inf, -Inf))
  expect_false(is_equal(Inf, -Inf))

  # NaN values
  expect_true(is_equal(NaN, NaN))
  expect_false(is_equal(NaN, 5))

  # Zero comparisons
  expect_true(is_equal(0, 0))
  expect_true(is_equal(0, -0))
  expect_true(is_equal(-0.000001, 0, tol = 0.0001))
})

test_that("is_equal validates tol parameter", {
  # Negative tolerance should error
  expect_error(
    is_equal(5, 5, tol = -1),
    "tol must be a single non-negative finite number"
  )

  # Inf tolerance should error
  expect_error(
    is_equal(5, 5, tol = Inf),
    "tol must be a single non-negative finite number"
  )

  # NA tolerance should error
  expect_error(
    is_equal(5, 5, tol = NA),
    "tol must be a single non-negative finite number"
  )

  # Vector tolerance should error
  expect_error(
    is_equal(5, 5, tol = c(1, 2)),
    "tol must be a single non-negative finite number"
  )

  # Non-numeric tolerance should error
  expect_error(
    is_equal(5, 5, tol = "0.1"),
    "tol must be a single non-negative finite number"
  )

  # Zero tolerance should work

  expect_true(is_equal(5, 5, tol = 0))
  expect_false(is_equal(5, 5.0001, tol = 0))
})
