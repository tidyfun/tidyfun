#!/usr/bin/env Rscript

# Test the specific case that might cause %+% warnings
devtools::load_all()
library(ggplot2)

# Load test helpers
source("tests/testthat/helper-tf-ggplot.R")

cat("=== Running specific test that might show %+% warnings ===\n")

set.seed(101112)
data <- create_test_tf_data(n_funcs = 4, n_points = 5)

# This is the exact code from test-tf-ggplot-complex.R line 118
p_mixed <- tf_ggplot(
  data,
  aes(
    tf = func,
    color = group, # Regular factor variable
    size = tf_fmean(func), # Scalar summary of tf
    alpha = abs(runif(4)) # Transformation of scalar
  )
) +
  suppressWarnings(geom_line())

cat("Plot created, now building...\n")

# Turn warnings into errors to see exactly where they come from
options(warn = 2)

try(
  {
    built_mixed <- ggplot_build(p_mixed)
    cat("Plot built successfully\n")
  },
  silent = FALSE
)
