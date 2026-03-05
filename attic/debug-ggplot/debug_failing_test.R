#!/usr/bin/env Rscript

# Debug the specific failing test case
devtools::load_all()
library(ggplot2)

# Load test helpers
source("tests/testthat/helper-tf-ggplot.R")

# Reproduce the failing test
set.seed(123)
data <- create_test_tf_data(n_funcs = 3, n_points = 5)
data$mean_val <- c(1, 2, 3)
data$max_val <- c(1.5, 2.5, 3.5)

cat("=== Reproducing failing test case ===\n")
cat("Data structure:\n")
str(data)

cat("\n=== Trying step by step ===\n")

# Step 1: tf_ggplot
cat("Step 1: Creating tf_ggplot...\n")
p <- tf_ggplot(data)
cat("tf_ggplot created successfully\n")

# Step 2: Add first tf layer
cat("Step 2: Adding first tf layer...\n")
p <- p + suppressWarnings(geom_line(aes(tf = func, color = group)))
cat("First tf layer added successfully\n")

# Step 3: Add first regular layer
cat("Step 3: Adding first regular layer...\n")
p <- p + geom_point(aes(x = 0.5, y = mean_val, color = group), size = 3)
cat("First regular layer added successfully\n")

# Step 4: Add second tf layer (this might fail)
cat("Step 4: Adding second tf layer...\n")
try(
  {
    p <- p + suppressWarnings(geom_point(aes(tf = func), alpha = 0.5))
    cat("Second tf layer added successfully\n")
  },
  silent = FALSE
)

cat("Step 5: Building plot...\n")
try(
  {
    built <- suppressWarnings(ggplot_build(p))
    cat("Plot built successfully\n")
    cat("Number of layers:", length(built$data), "\n")
    for (i in seq_along(built$data)) {
      cat("Layer", i, "rows:", nrow(built$data[[i]]), "\n")
    }
  },
  silent = FALSE
)
