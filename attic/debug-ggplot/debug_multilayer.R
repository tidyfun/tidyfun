#!/usr/bin/env Rscript

# Debug script to understand multi-layer issue
devtools::load_all()
library(ggplot2)

# Create simple test data
set.seed(123)
data <- data.frame(id = 1:2, group = factor(c("A", "B")))
data$func <- tf_rgp(2, arg = seq(0, 1, length.out = 5))

cat("=== Testing single tf layer (should work) ===\n")
try({
  p1 <- tf_ggplot(data, aes(tf = func, color = group)) + geom_line()
  print("Single layer created successfully")
  built1 <- suppressWarnings(ggplot_build(p1))
  cat("Single layer built successfully - rows:", nrow(built1$data[[1]]), "\n")
})

cat("\n=== Testing two tf layers (might fail) ===\n")
try({
  p2 <- tf_ggplot(data, aes(tf = func, color = group)) +
    geom_line() +
    geom_point()
  print("Two layers created successfully")
  built2 <- suppressWarnings(ggplot_build(p2))
  cat("Two layers built successfully\n")
  cat("Layer 1 rows:", nrow(built2$data[[1]]), "\n")
  cat("Layer 2 rows:", nrow(built2$data[[2]]), "\n")
})

cat("\n=== Testing two tf layers with explicit aes (might fail) ===\n")
try({
  p3 <- tf_ggplot(data) +
    geom_line(aes(tf = func, color = group)) +
    geom_point(aes(tf = func))
  print("Two explicit layers created successfully")
  built3 <- suppressWarnings(ggplot_build(p3))
  cat("Two explicit layers built successfully\n")
  cat("Layer 1 rows:", nrow(built3$data[[1]]), "\n")
  cat("Layer 2 rows:", nrow(built3$data[[2]]), "\n")
})
