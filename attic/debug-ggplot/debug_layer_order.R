#!/usr/bin/env Rscript

# Debug layer ordering
devtools::load_all()
library(ggplot2)

# Load test helpers
source("tests/testthat/helper-tf-ggplot.R")

set.seed(123)
data <- create_test_tf_data(n_funcs = 2, n_points = 5)
data$mean_val <- c(1, 2)

cat("=== Testing layer order ===\n")

# The expected order should be:
# 1. tf_ggplot(data) +
# 2. geom_line(aes(tf = func, color = group)) +     # tf layer 1
# 3. geom_point(aes(x = 0.5, y = mean_val), size = 3) +  # regular layer 1
# 4. geom_point(aes(tf = func), alpha = 0.5)        # tf layer 2

p <- tf_ggplot(data) +
  suppressWarnings(geom_line(aes(tf = func, color = group))) + # Should be layer 1 in final plot
  geom_point(aes(x = 0.5, y = mean_val), size = 3) + # Should be layer 2 in final plot
  suppressWarnings(geom_point(aes(tf = func), alpha = 0.5)) # Should be layer 3 in final plot

built <- suppressWarnings(ggplot_build(p))

cat("Number of layers:", length(built$data), "\n")
for (i in seq_along(built$data)) {
  layer_data <- built$data[[i]]
  cat("Layer", i, "rows:", nrow(layer_data), "\n")

  # Check if this is a tf layer (has group column with multiple values) or regular layer
  if ("group" %in% names(layer_data) && length(unique(layer_data$group)) > 1) {
    cat("  -> This appears to be a tf layer\n")
  } else {
    cat("  -> This appears to be a regular layer\n")
  }
}

# Expected:
# Layer 1: tf layer (10 rows: 2 functions × 5 points) - geom_line
# Layer 2: regular layer (2 rows) - geom_point with mean_val
# Layer 3: tf layer (10 rows: 2 functions × 5 points) - geom_point tf
