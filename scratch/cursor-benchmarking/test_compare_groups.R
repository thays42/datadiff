#!/usr/bin/env Rscript

# Test script to verify both compare_groups functions produce the same results

library(datadiff)
library(dplyr)

# Create simple test data
set.seed(123)
test_data <- data.frame(
  group1 = rep(letters[1:5], each = 4),
  group2 = rep(1:4, 5),
  value = rnorm(20)
)

# Create two datasets with some differences
x_data <- test_data[1:15, ]
y_data <- test_data[6:20, ]

cat("Test data summary:\n")
cat("x_data has", nrow(x_data), "rows\n")
cat("y_data has", nrow(y_data), "rows\n")
cat("Expected overlap:", length(intersect(1:15, 6:20)), "rows\n\n")

# Test both functions
result_full_join <- compare_groups(x_data, y_data, c(group1, group2))
result_anti_join <- compare_groups_anti_join(x_data, y_data, c(group1, group2))

cat("Results from full_join approach:\n")
print(result_full_join)
cat("\nResults from anti_join approach:\n")
print(result_anti_join)

# Check if results are identical
are_identical <- all.equal(result_full_join, result_anti_join)
cat("\nResults are identical:", are_identical, "\n")

if (isTRUE(are_identical)) {
  cat(
    "✓ Both functions produce the same results. Proceeding with benchmark...\n"
  )
} else {
  cat("✗ Functions produce different results. Check implementation.\n")
  stop("Test failed")
}
