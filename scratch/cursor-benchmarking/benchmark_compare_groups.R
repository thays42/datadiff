#!/usr/bin/env Rscript

# Benchmark script to compare compare_groups vs compare_groups_anti_join
# Test with moderate sized data in the 100,000s of observations range

library(datadiff)
library(dplyr)
library(bench)
library(ggplot2)

# Set seed for reproducible results
set.seed(123)

# Generate test data
generate_test_data <- function(n_obs, n_groups, overlap_ratio = 0.8) {
  # Create group combinations
  group1_vals <- paste0("group1_", 1:n_groups)
  group2_vals <- paste0("group2_", 1:n_groups)

  # Create data frame with groups
  data <- expand.grid(
    group1 = group1_vals,
    group2 = group2_vals,
    stringsAsFactors = FALSE
  )

  # Add some additional columns
  data$value1 <- rnorm(nrow(data))
  data$value2 <- sample(letters, nrow(data), replace = TRUE)

  # Sample to get desired number of observations
  if (nrow(data) < n_obs) {
    # If we need more rows, duplicate some
    data <- data[rep(1:nrow(data), ceiling(n_obs / nrow(data))), ]
  }

  # Sample to exact size
  data <- data[sample(nrow(data), n_obs, replace = FALSE), ]

  # Create two datasets with some overlap
  n_overlap <- floor(n_obs * overlap_ratio)
  n_x_only <- n_obs - n_overlap
  n_y_only <- n_obs - n_overlap

  # Ensure we have enough rows to sample non-overlapping indices
  total_needed <- n_overlap + n_x_only + n_y_only
  if (nrow(data) < total_needed) {
    # If not enough, allow replacement
    indices <- sample(nrow(data), total_needed, replace = TRUE)
  } else {
    indices <- sample(nrow(data), total_needed, replace = FALSE)
  }
  overlap_indices <- indices[1:n_overlap]
  x_only_indices <- indices[(n_overlap + 1):(n_overlap + n_x_only)]
  y_only_indices <- indices[
    (n_overlap + n_x_only + 1):(n_overlap + n_x_only + n_y_only)
  ]

  x_data <- data[c(overlap_indices, x_only_indices), ]
  y_data <- data[c(overlap_indices, y_only_indices), ]

  list(x = x_data, y = y_data)
}

# Alternative implementation using anti_joins and bind_rows
compare_groups_anti_join <- function(x, y, group_cols) {
  x_groups <- x |>
    dplyr::select({{ group_cols }}) |>
    dplyr::distinct() |>
    dplyr::mutate(in_x = TRUE, in_y = FALSE)
  y_groups <- y |>
    dplyr::select({{ group_cols }}) |>
    dplyr::distinct() |>
    dplyr::mutate(in_x = FALSE, in_y = TRUE)

  # Find groups only in x (not in y)
  x_only <- dplyr::anti_join(
    x_groups,
    y_groups,
    by = names(x_groups)[!names(x_groups) %in% c("in_x", "in_y")]
  )

  # Find groups only in y (not in x)
  y_only <- dplyr::anti_join(
    y_groups,
    x_groups,
    by = names(y_groups)[!names(y_groups) %in% c("in_x", "in_y")]
  )

  # Combine results
  dplyr::bind_rows(x_only, y_only) |>
    dplyr::arrange(dplyr::pick(names(x_groups)[
      !names(x_groups) %in% c("in_x", "in_y")
    ]))
}

# Run benchmarks
run_benchmarks <- function() {
  # Test with different data sizes
  sizes <- c(50000, 100000, 200000, 500000)
  results <- list()

  for (size in sizes) {
    cat("Testing with", size, "observations...\n")

    # Generate test data
    test_data <- generate_test_data(size, 100)

    # Benchmark both functions
    bm <- bench::mark(
      full_join = compare_groups(test_data$x, test_data$y, c(group1, group2)),
      anti_join = compare_groups_anti_join(
        test_data$x,
        test_data$y,
        c(group1, group2)
      ),
      iterations = 10,
      check = FALSE # Disable checking for speed
    )

    results[[as.character(size)]] <- bm
  }

  results
}

# Run the benchmarks
cat("Starting benchmarks...\n")
benchmark_results <- run_benchmarks()

# Print results
cat("\n=== BENCHMARK RESULTS ===\n")
for (size in names(benchmark_results)) {
  cat("\nData size:", size, "observations\n")
  print(benchmark_results[[size]])
}

# Create summary plot
cat("\n=== PERFORMANCE SUMMARY ===\n")
summary_data <- data.frame()
for (size in names(benchmark_results)) {
  bm <- benchmark_results[[size]]
  for (i in 1:nrow(bm)) {
    summary_data <- rbind(
      summary_data,
      data.frame(
        size = as.numeric(size),
        method = bm$expression[i],
        median_time = bm$median[i],
        mean_time = bm$mean[i],
        min_time = bm$min[i],
        max_time = bm$max[i]
      )
    )
  }
}

# Print summary table
cat("\nPerformance Summary (median times in milliseconds):\n")
print(summary_data)

# Calculate speedup
cat("\n=== SPEEDUP ANALYSIS ===\n")
for (size in unique(summary_data$size)) {
  size_data <- summary_data[summary_data$size == size, ]
  full_join_time <- size_data$median_time[size_data$method == "full_join"]
  anti_join_time <- size_data$median_time[size_data$method == "anti_join"]

  speedup <- full_join_time / anti_join_time
  cat(sprintf(
    "Size %d: anti_join is %.2fx %s than full_join\n",
    size,
    speedup,
    ifelse(speedup > 1, "faster", "slower")
  ))
}

# Memory usage comparison
cat("\n=== MEMORY USAGE ===\n")
for (size in names(benchmark_results)) {
  bm <- benchmark_results[[size]]
  cat("\nData size:", size, "observations\n")
  for (i in 1:nrow(bm)) {
    cat(sprintf(
      "%s: %.2f MB allocated\n",
      bm$expression[i],
      bm$mem_alloc[i] / 1024^2
    ))
  }
}

cat("\nBenchmark completed!\n")
