#!/usr/bin/env Rscript

# Debug the tf aesthetic parsing
devtools::load_all()
library(ggplot2)

# Load test helpers
source("tests/testthat/helper-tf-ggplot.R")

set.seed(123)
data <- create_test_tf_data(n_funcs = 3, n_points = 5)

cat("=== Testing tf aesthetic parsing ===\n")

# Test 1: Simple tf aesthetic
mapping1 <- aes(tf = func)
parsed1 <- parse_tf_aesthetics(mapping1, data)
cat("mapping1 (tf = func):\n")
str(parsed1)

# Test 2: tf aesthetic with other aesthetics
mapping2 <- aes(tf = func, color = group)
parsed2 <- parse_tf_aesthetics(mapping2, data)
cat("\nmapping2 (tf = func, color = group):\n")
str(parsed2)

# Test 3: tf aesthetic with alpha
mapping3 <- aes(tf = func, alpha = 0.5)
parsed3 <- parse_tf_aesthetics(mapping3, data)
cat("\nmapping3 (tf = func, alpha = 0.5):\n")
str(parsed3)

# Test manual layer creation to see what's happening
cat("\n=== Testing layer creation ===\n")
try(
  {
    # Create tf_ggplot with no layers
    p <- tf_ggplot(data)
    cat("tf_layers after creating plot:", length(attr(p, "tf_layers")), "\n")

    # Add first layer
    layer1 <- geom_line(aes(tf = func, color = group))
    p <- p + suppressWarnings(layer1)
    cat("tf_layers after first layer:", length(attr(p, "tf_layers")), "\n")

    # Add second layer
    layer2 <- geom_point(aes(tf = func), alpha = 0.5)
    cat("About to add second layer...\n")

    # Let's see what parse_tf_aesthetics returns for this layer
    layer2_mapping <- layer2$mapping %||% aes()
    cat("layer2 mapping:\n")
    str(layer2_mapping)

    layer2_parsed <- parse_tf_aesthetics(layer2_mapping, data)
    cat("layer2 parsed aesthetics:\n")
    str(layer2_parsed)

    p <- p + suppressWarnings(layer2)
    cat("tf_layers after second layer:", length(attr(p, "tf_layers")), "\n")

    # Check what's stored in tf_layers
    tf_layers <- attr(p, "tf_layers")
    for (i in seq_along(tf_layers)) {
      cat(
        "Layer",
        i,
        "tf_aes count:",
        length(tf_layers[[i]]$parsed_aes$tf_aes),
        "\n"
      )
      cat(
        "Layer",
        i,
        "tf_aes names:",
        names(tf_layers[[i]]$parsed_aes$tf_aes),
        "\n"
      )
    }
  },
  silent = FALSE
)
