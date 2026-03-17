# Tests for aesthetic specification equivalence in tf_ggplot
# Verifies that aesthetics can be specified in constructor or geom layer with identical results

test_that("tf aesthetics in constructor vs geom layer are equivalent - basic case", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)

  # Method 1: tf aesthetic in constructor
  p1 <- tf_ggplot(data, aes(tf = func)) + geom_line()

  # Method 2: tf aesthetic in geom layer
  p2 <- tf_ggplot(data) + suppressWarnings(geom_line(aes(tf = func)))

  # Build both plots
  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should have identical data structure
  expect_equal(nrow(built1$data[[1]]), nrow(built2$data[[1]]))
  expect_equal(ncol(built1$data[[1]]), ncol(built2$data[[1]]))
  expect_equal(names(built1$data[[1]]), names(built2$data[[1]]))

  # Should have same number of groups
  expect_equal(
    length(unique(built1$data[[1]]$group)),
    length(unique(built2$data[[1]]$group))
  )

  # x and y values should be identical (allowing for floating point differences)
  expect_equal(sort(built1$data[[1]]$x), sort(built2$data[[1]]$x))
  expect_equal(
    sort(built1$data[[1]]$y),
    sort(built2$data[[1]]$y),
    tolerance = 1e-10
  )
})

test_that("tf aesthetics with color mapping are equivalent", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 4, n_points = 6)

  # Method 1: aesthetics in constructor
  p1 <- tf_ggplot(data, aes(tf = func, color = group)) + geom_line()

  # Method 2: aesthetics in geom layer
  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_line(aes(tf = func, color = group)))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should have identical structure
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))

  # Color mappings should be identical
  expect_equal(sort(built1$data[[1]]$colour), sort(built2$data[[1]]$colour))

  # Groups should be identical
  expect_equal(sort(built1$data[[1]]$group), sort(built2$data[[1]]$group))

  # Check that color is consistent within groups for both methods
  check_color_consistency <- function(plot_data) {
    color_by_group <- split(plot_data$colour, plot_data$group)
    all(sapply(color_by_group, function(x) length(unique(x)) == 1))
  }

  expect_true(check_color_consistency(built1$data[[1]]))
  expect_true(check_color_consistency(built2$data[[1]]))
})

test_that("mixed tf and regular aesthetics are equivalent", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)
  data$size_var <- c(1, 2, 3)
  data$alpha_var <- c(0.3, 0.7, 0.9)

  # Method 1: mixed aesthetics in constructor
  p1 <- tf_ggplot(
    data,
    aes(tf = func, color = group, size = size_var, alpha = alpha_var)
  ) +
    geom_line()

  # Method 2: mixed aesthetics in geom layer
  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_line(aes(
      tf = func,
      color = group,
      size = size_var,
      alpha = alpha_var
    )))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should have identical dimensions and structure
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))
  expect_setequal(names(built1$data[[1]]), names(built2$data[[1]]))

  # All aesthetic mappings should be identical
  aesthetic_cols <- c("colour", "size", "alpha")
  for (col in aesthetic_cols) {
    expect_equal(
      sort(built1$data[[1]][[col]]),
      sort(built2$data[[1]][[col]]),
      info = paste("Mismatch in", col, "aesthetic")
    )
  }
})

test_that("multiple tf aesthetics are equivalent", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_multi_tf_data(n_funcs = 2, n_points = 4)

  # Method 1: multiple tf aesthetics in constructor
  p1 <- tf_ggplot(data, aes(tf_x = func1, tf_y = func2)) + geom_point()

  # Method 2: multiple tf aesthetics in geom layer
  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_point(aes(tf_x = func1, tf_y = func2)))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should have identical structure
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))

  # x and y coordinates should be identical
  expect_equal(sort(built1$data[[1]]$x), sort(built2$data[[1]]$x))
  expect_equal(sort(built1$data[[1]]$y), sort(built2$data[[1]]$y))

  # Groups should be identical
  expect_equal(sort(built1$data[[1]]$group), sort(built2$data[[1]]$group))
})

test_that("ribbon tf aesthetics are equivalent", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_band_tf_data(n_funcs = 2, n_points = 5)

  # Method 1: ribbon aesthetics in constructor
  p1 <- tf_ggplot(data, aes(tf_ymin = lower_func, tf_ymax = upper_func)) +
    geom_ribbon(alpha = 0.3)

  # Method 2: ribbon aesthetics in geom layer
  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_ribbon(
      aes(tf_ymin = lower_func, tf_ymax = upper_func),
      alpha = 0.3
    ))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should have identical structure
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))

  # ymin and ymax should be identical
  expect_equal(sort(built1$data[[1]]$ymin), sort(built2$data[[1]]$ymin))
  expect_equal(sort(built1$data[[1]]$ymax), sort(built2$data[[1]]$ymax))

  # Alpha should be identical
  expect_equal(built1$data[[1]]$alpha, built2$data[[1]]$alpha)
})

test_that("aesthetic inheritance and override work correctly", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)

  # Base aesthetics in constructor, additional in geom
  p1 <- tf_ggplot(data, aes(tf = func)) +
    geom_line(aes(color = group))

  # All aesthetics in geom layer
  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_line(aes(tf = func, color = group)))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should produce identical results
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))
  expect_equal(sort(built1$data[[1]]$colour), sort(built2$data[[1]]$colour))
  expect_equal(sort(built1$data[[1]]$x), sort(built2$data[[1]]$x))
  expect_equal(sort(built1$data[[1]]$y), sort(built2$data[[1]]$y))
})

test_that("aesthetic override in geom layer works", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 5)
  data$alt_group <- factor(c("X", "Y", "Z"))

  # Constructor aesthetic overridden in geom
  p1 <- tf_ggplot(data, aes(tf = func, color = group)) +
    geom_line(aes(color = alt_group)) # Override color aesthetic

  # Direct specification in geom
  p2 <- tf_ggplot(data, aes(tf = func)) +
    geom_line(aes(color = alt_group))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should use alt_group for coloring in both cases
  expect_equal(sort(built1$data[[1]]$colour), sort(built2$data[[1]]$colour))

  # Should have 3 unique colors (X, Y, Z)
  expect_equal(length(unique(built1$data[[1]]$colour)), 3)
  expect_equal(length(unique(built2$data[[1]]$colour)), 3)
})

test_that("faceting works equivalently with different aesthetic specifications", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 6, n_points = 4)
  data$treatment <- factor(rep(c("A", "B"), each = 3))

  # Method 1: aesthetics in constructor
  p1 <- tf_ggplot(data, aes(tf = func)) +
    geom_line() +
    facet_wrap(~treatment)

  # Method 2: aesthetics in geom layer
  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_line(aes(tf = func))) +
    facet_wrap(~treatment)

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should have identical panel structure
  expect_equal(dim(built1$layout$layout), dim(built2$layout$layout))
  expect_equal(built1$layout$layout$PANEL, built2$layout$layout$PANEL)

  # Plot data should be identical
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))
  expect_equal(sort(built1$data[[1]]$PANEL), sort(built2$data[[1]]$PANEL))
})

test_that("custom arg parameter works equivalently", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 11)
  custom_arg <- seq(0, 1, length.out = 6)

  # Method 1: aesthetics in constructor with custom arg
  p1 <- tf_ggplot(data, aes(tf = func), arg = custom_arg) + geom_line()

  # Method 2: aesthetics in geom layer with custom arg
  p2 <- tf_ggplot(data, arg = custom_arg) +
    suppressWarnings(geom_line(aes(tf = func)))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should use custom arg in both cases
  expect_equal(sort(unique(built1$data[[1]]$x)), custom_arg)
  expect_equal(sort(unique(built2$data[[1]]$x)), custom_arg)

  # Should have identical structure
  expect_equal(dim(built1$data[[1]]), dim(built2$data[[1]]))
  expect_equal(sort(built1$data[[1]]$x), sort(built2$data[[1]]$x))
  expect_equal(sort(built1$data[[1]]$y), sort(built2$data[[1]]$y))
})

test_that("multiple layers with different aesthetic specifications work", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 2, n_points = 5)

  # Mix of constructor and geom-level aesthetics across multiple layers
  p1 <- tf_ggplot(data, aes(tf = func)) +
    geom_line(aes(color = group)) +
    geom_point(aes(color = group), size = 2)

  # All aesthetics specified at geom level
  p2 <- tf_ggplot(data) +
    suppressWarnings(geom_line(aes(tf = func, color = group))) +
    suppressWarnings(geom_point(aes(tf = func, color = group), size = 2))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Should have same number of layers
  expect_equal(length(built1$data), length(built2$data))

  # Each layer should be identical
  for (i in seq_along(built1$data)) {
    expect_equal(
      dim(built1$data[[i]]),
      dim(built2$data[[i]]),
      info = paste("Layer", i, "dimensions differ")
    )
    expect_equal(
      sort(built1$data[[i]]$x),
      sort(built2$data[[i]]$x),
      info = paste("Layer", i, "x values differ")
    )
    expect_equal(
      sort(built1$data[[i]]$y),
      sort(built2$data[[i]]$y),
      info = paste("Layer", i, "y values differ")
    )
    expect_equal(
      sort(built1$data[[i]]$colour),
      sort(built2$data[[i]]$colour),
      info = paste("Layer", i, "color values differ")
    )
  }
})

test_that("data transformation is identical regardless of aesthetic specification method", {
  skip_if_not_installed("ggplot2")
  skip_if_no_tf_ggplot()

  data <- create_test_tf_data(n_funcs = 3, n_points = 7)
  data$extra_var <- letters[1:3]

  # Complex aesthetic mapping
  aesthetic_mapping <- aes(tf = func, color = group, linetype = extra_var)

  # Method 1: in constructor
  p1 <- tf_ggplot(data, aesthetic_mapping) + geom_line()

  # Method 2: in geom layer
  p2 <- tf_ggplot(data) + suppressWarnings(geom_line(aesthetic_mapping))

  built1 <- suppressWarnings(ggplot_build(p1))
  built2 <- suppressWarnings(ggplot_build(p2))

  # Extract and sort all data for comparison
  data1 <- built1$data[[1]][order(built1$data[[1]]$group, built1$data[[1]]$x), ]
  data2 <- built2$data[[1]][order(built2$data[[1]]$group, built2$data[[1]]$x), ]

  # Reset row names for comparison
  rownames(data1) <- NULL
  rownames(data2) <- NULL

  # Should be completely identical
  common_cols <- sort(intersect(names(data1), names(data2)))
  expect_equal(data1[common_cols], data2[common_cols])
})
