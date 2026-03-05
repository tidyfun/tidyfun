#!/usr/bin/env Rscript

# Test without deprecated aesthetics
devtools::load_all()
library(ggplot2)

# Load test helpers
source("tests/testthat/helper-tf-ggplot.R")

cat("=== ggplot2 version ===\n")
cat("ggplot2 version:", as.character(packageVersion("ggplot2")), "\n")

cat("=== Running clean test ===\n")

set.seed(101112)
data <- create_test_tf_data(n_funcs = 4, n_points = 5)

# Use linewidth instead of size to avoid deprecation warnings
p_mixed <- tf_ggplot(
  data,
  aes(
    tf = func,
    color = group, # Regular factor variable
    linewidth = tf_fmean(func), # Scalar summary of tf
    alpha = abs(runif(4)) # Transformation of scalar
  )
) +
  suppressWarnings(geom_line())

cat("Plot created, now building...\n")

# Check for actual %+% warnings
old_warn <- options(warn = 1)$warn
on.exit(options(warn = old_warn))

built_mixed <- ggplot_build(p_mixed)
cat("Plot built successfully\n")
