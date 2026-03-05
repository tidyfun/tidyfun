#!/usr/bin/env Rscript

# Debug the finalization process
devtools::load_all()
library(ggplot2)

# Load test helpers
source("tests/testthat/helper-tf-ggplot.R")

set.seed(123)
data <- create_test_tf_data(n_funcs = 3, n_points = 5)

cat("=== Testing finalization process ===\n")

# Create the problematic plot setup
p <- tf_ggplot(data) +
  suppressWarnings(geom_line(aes(tf = func, color = group))) +
  geom_point(aes(x = 0.5, y = c(1, 2, 3), color = group), size = 3) +
  suppressWarnings(geom_point(aes(tf = func), alpha = 0.5))

cat("Plot created with 3 layers\n")
cat("tf_layers count:", length(attr(p, "tf_layers")), "\n")

# Let's manually call finalize_tf_ggplot to see what happens
cat("\n=== Calling finalize_tf_ggplot ===\n")
try(
  {
    # This is the internal function that should be called during ggplot_build
    finalized <- tidyfun:::finalize_tf_ggplot(p)
    cat("Finalization successful!\n")
    cat("Number of layers in finalized plot:", length(finalized$layers), "\n")
  },
  silent = FALSE
)
