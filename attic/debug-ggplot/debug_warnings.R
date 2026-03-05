#!/usr/bin/env Rscript

# Debug the %+% warnings
devtools::load_all()
library(ggplot2)

# Load test helpers
source("tests/testthat/helper-tf-ggplot.R")

set.seed(123)
data <- create_test_tf_data(n_funcs = 2, n_points = 5)

cat("=== Testing for %+% warnings ===\n")

# Try a complex plot that might trigger the warning
options(warn = 2) # Convert warnings to errors to see exactly where they come from

try(
  {
    p <- tf_ggplot(data, aes(tf = func, color = group)) +
      geom_line() +
      scale_color_manual(values = c("A" = "red", "B" = "blue")) + # This might trigger %+%
      theme_minimal()

    built <- ggplot_build(p)
    cat("Plot built successfully without warnings\n")
  },
  silent = FALSE
)

options(warn = 0) # Reset warnings
